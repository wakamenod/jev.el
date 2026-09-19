;;; jev-http.el --- HTTP transport for the Jev client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wakamenod

;; Author: wakamenod <wakamenod@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Transport layer for `jev.el'.  A request is one HTTP/1.1 exchange
;; over a socket this file opens, writes and reads itself, on
;; `make-network-process' and nothing else.  It produces a normalised
;; result plist:
;;
;;   (:status CODE :headers ALIST :body STRING)
;;   (:error SYMBOL :message STRING)        ; `connection' or `timeout'
;;
;; A 3xx is a status like any other: nothing here follows it, so the
;; Authorization header never travels anywhere but the endpoint it
;; was written for.  Nothing prompts, nothing is pooled, and nothing
;; depends on url.el, whose behaviour differs between the Emacs
;; versions this package supports.
;;
;; Retries for 408, 429 and 5xx responses (529 included) and for
;; transport failures live here too, so the layer above only ever
;; sees a final result.
;;
;; `jev-http-function' replaces the transport wholesale: it is how the
;; tests stub the network, and how anything this client cannot reach
;; -- a proxy, a client certificate -- is plugged in.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'gnutls)
(require 'jev-core)


(defvar jev-http-function nil
  "When non-nil, a function replacing the HTTP transport.
Called with (URL HEADERS BODY TIMEOUT SYNC CALLBACK).  When SYNC is
non-nil it returns a result plist, otherwise it calls CALLBACK with
one, and may return a function that abandons the attempt.
Intended for tests.")

;; This one is handed the URL, the body and the headers -- the API key
;; among them -- and may do anything with them, so a file-local must
;; not be able to set it quietly.
(put 'jev-http-function 'risky-local-variable t)


;;;; One exchange over a socket

(cl-defstruct (jev-http--exchange (:constructor jev-http--make-exchange) (:copier nil))
  "One request and the reply to it, in flight.
SETTLE is called with the result plist, once; an exchange that is
abandoned settles with nothing and calls nobody."
  target process buffer request settle timer done)

(defun jev-http--target (url)
  "Return where URL points as a plist of :host, :port, :tls and :path.
Signal an `error' for anything that is not an HTTP URL."
  (let* ((parsed (url-generic-parse-url url))
         (scheme (url-type parsed))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (unless (and (member scheme '("http" "https")) host (not (string-empty-p host)))
      (error "Not an HTTP URL: %s" url))
    (list :host host
          :port (or (url-portspec parsed) (if (equal scheme "https") 443 80))
          :tls (equal scheme "https")
          :path (if (string-empty-p path) "/" path))))

(defun jev-http--request-text (target headers body)
  "Return the HTTP/1.1 request carrying BODY to TARGET with HEADERS.
A unibyte string, ready for a binary connection."
  (let ((data (encode-coding-string body 'utf-8))
        (host (plist-get target :host))
        (port (plist-get target :port)))
    (concat
     (encode-coding-string
      (concat
       (format "POST %s HTTP/1.1\r\n" (plist-get target :path))
       (format "Host: %s\r\n"
               (if (eq port (if (plist-get target :tls) 443 80)) host
                 (format "%s:%d" host port)))
       ;; One exchange per connection: the reply is complete when the
       ;; framing says so, or when the server hangs up, whichever
       ;; comes first, and nothing is left for anyone to reuse.
       "Connection: close\r\n"
       (format "Content-Length: %d\r\n" (length data))
       (mapconcat (lambda (header) (format "%s: %s\r\n" (car header) (cdr header)))
                  headers "")
       "\r\n")
      'utf-8)
     data)))

(defun jev-http--open (target buffer filter sentinel)
  "Start connecting to TARGET; return the process.

BUFFER, FILTER and SENTINEL are the process's.  The connection is
made asynchronously: SENTINEL hears `open' once it is up and
`failed' when it never will be.  It comes up as plain TCP even
for an https URL; `jev-http--negotiate' puts TLS on it then."
  (when (and (plist-get target :tls) (not (gnutls-available-p)))
    (error "This Emacs has no GnuTLS, so it cannot reach %s over TLS"
           (plist-get target :host)))
  (make-network-process
   :name "jev" :buffer buffer
   :host (plist-get target :host) :service (plist-get target :port)
   :nowait t :coding 'binary :noquery t
   :filter filter :sentinel sentinel))

(defun jev-http--negotiate (target process)
  "Put TLS on the freshly opened PROCESS to TARGET, verifying its certificate.

Synchronously, on purpose: the asynchronous handshake Emacs
offers through `:tls-parameters' does not refuse a certificate
that fails to verify, whatever `:verify-error' says, and
`gnutls-negotiate' does.  The cost is a handshake's worth of
blocking, for an https URL only.  A failure is a failed
connection, never a question for the user."
  (let ((host (plist-get target :host)))
    (condition-case err
        (gnutls-negotiate :process process :type 'gnutls-x509pki
                          :hostname host :verify-error t)
      (error
       ;; On a connection that was opened asynchronously GnuTLS
       ;; says why in the process status, and the error object only
       ;; names the process; the status is the readable one.
       (let ((why (process-exit-status process)))
         (error "TLS with %s failed: %s" host
                (if (stringp why) why (error-message-string err))))))))

(defun jev-http--connection-error (message)
  "Return a result plist for a transport failure described by MESSAGE."
  (list :error 'connection :message message))

(defun jev-http--settle (exchange result)
  "Finish EXCHANGE and report RESULT to its callback.
Everything the exchange holds is taken down first: the callback
runs on a request that is over.  A nil RESULT reports nothing,
which is how an exchange is abandoned.  Settling twice is
harmless, so every path that ends an exchange may call this."
  (unless (jev-http--exchange-done exchange)
    (setf (jev-http--exchange-done exchange) t)
    (when-let* ((timer (jev-http--exchange-timer exchange)))
      (cancel-timer timer))
    (let ((process (jev-http--exchange-process exchange))
          (buffer (jev-http--exchange-buffer exchange)))
      (when (processp process)
        (set-process-sentinel process #'ignore)
        (set-process-filter process #'ignore)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (when result
      (funcall (jev-http--exchange-settle exchange) result))))

(defun jev-http--status-code (head)
  "Return the status code of the response whose header block is HEAD, or nil."
  (and (string-match "\\`HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)" head)
       (string-to-number (match-string 1 head))))

(defun jev-http--parse-headers (text)
  "Parse the raw header block TEXT into an alist.
The first line is the status line, which is not a header."
  (let (out)
    (dolist (line (cdr (split-string text "\r?\n" t)) (nreverse out))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (push (cons (downcase (match-string 1 line))
                    (string-trim (match-string 2 line)))
              out)))))

(defun jev-http--dechunk (data closed)
  "Return the body that the chunked DATA carries.
Nil while more is due -- or, when the connection has CLOSED
without the last chunk, `truncated' -- and `malformed' for
something that is not chunked encoding at all."
  (let ((pos 0) (parts nil) (state nil))
    (while (null state)
      (let ((eol (string-match "\r\n" data pos)))
        (if (null eol)
            (setq state 'incomplete)
          (let ((line (substring data pos eol)))
            (if (not (string-match "\\`\\([0-9a-fA-F]+\\)[ \t]*\\(;.*\\)?\\'" line))
                (setq state 'malformed)
              (let* ((size (string-to-number (match-string 1 line) 16))
                     (start (+ eol 2))
                     (end (+ start size)))
                (cond
                 ((zerop size) (setq state 'complete))
                 ((> (+ end 2) (length data)) (setq state 'incomplete))
                 ((not (equal (substring data end (+ end 2)) "\r\n"))
                  (setq state 'malformed))
                 (t (push (substring data start end) parts)
                    (setq pos (+ end 2))))))))))
    (pcase state
      ('complete (apply #'concat (nreverse parts)))
      ('incomplete (and closed 'truncated))
      (_ state))))

(defun jev-http--body (headers raw closed)
  "Return the body the response carries, given its HEADERS and RAW bytes so far.
Nil while more is due; `truncated' or `malformed' when there will
never be a whole one; CLOSED says the connection is gone."
  (let ((transfer (alist-get "transfer-encoding" headers nil nil #'equal))
        (declared (alist-get "content-length" headers nil nil #'equal)))
    (cond
     ((and transfer (string-match-p "chunked" transfer))
      (jev-http--dechunk raw closed))
     (declared
      (let ((size (string-to-number declared)))
        (cond ((>= (length raw) size) (substring raw 0 size))
              (closed 'truncated))))
     ;; Neither: the body ends where the connection does, which is
     ;; what `Connection: close' in the request asked for anyway.
     (closed raw))))

(defun jev-http--parse (buffer closed)
  "Return the result plist for the response in BUFFER.
Nil while it is still incomplete.  CLOSED says the connection is
gone, so what is there is all there will be: a response that is
still incomplete then is a transport failure, and worth another
attempt."
  (with-current-buffer buffer
    (catch 'parsed
      (while t
        (goto-char (point-min))
        (unless (re-search-forward "\r?\n\r?\n" nil t)
          (throw 'parsed
                 (and closed
                      (jev-http--connection-error
                       "The Jev API closed the connection before answering"))))
        (let* ((head (buffer-substring-no-properties (point-min) (match-beginning 0)))
               (status (jev-http--status-code head)))
          (cond
           ((null status)
            (throw 'parsed (jev-http--connection-error
                            "The Jev API answered with something that is not HTTP")))
           ;; An interim reply; the real one follows it.
           ((< status 200)
            (delete-region (point-min) (point)))
           (t
            (let* ((headers (jev-http--parse-headers head))
                   (body (jev-http--body
                          headers (buffer-substring-no-properties (point) (point-max))
                          closed)))
              (throw 'parsed
                     (pcase body
                       ('nil nil)
                       ('truncated (jev-http--connection-error
                                    "The Jev API closed the connection mid-response"))
                       ('malformed (jev-http--connection-error
                                    "The Jev API sent a body this client could not read"))
                       (_ (list :status status :headers headers
                                :body (string-trim
                                       (decode-coding-string body 'utf-8))))))))))))))

(defun jev-http--conclude (exchange closed)
  "Settle EXCHANGE if its buffer holds a whole response.
When the connection has CLOSED, settle it with whatever it holds."
  (when-let* ((result (jev-http--parse (jev-http--exchange-buffer exchange) closed)))
    (jev-http--settle exchange result)))

(defun jev-http--on-output (exchange data)
  "Add DATA, read from the socket, to EXCHANGE.
Settle the exchange if that completes the reply."
  (unless (jev-http--exchange-done exchange)
    (with-current-buffer (jev-http--exchange-buffer exchange)
      (goto-char (point-max))
      (insert data))
    (jev-http--conclude exchange nil)))

(defun jev-http--on-event (exchange process event)
  "React to EVENT on the PROCESS of EXCHANGE.
The connection coming up is when the request is written; it going
down is when the reply is read, or found wanting."
  (unless (jev-http--exchange-done exchange)
    (cond
     ((string-prefix-p "open" event)
      (condition-case err
          (let ((target (jev-http--exchange-target exchange)))
            (when (plist-get target :tls)
              (jev-http--negotiate target process))
            (process-send-string process (jev-http--exchange-request exchange)))
        (error
         (jev-http--settle exchange
                           (jev-http--connection-error
                            (format "Could not send the request: %s"
                                    (error-message-string err)))))))
     ((process-live-p process))
     ((eq (process-status process) 'failed)
      (jev-http--settle exchange
                        (jev-http--connection-error
                         (format "Could not connect to %s: %s"
                                 (plist-get (process-contact process t) :host)
                                 (string-trim event)))))
     (t (jev-http--conclude exchange t)))))

(defun jev-http--start (url headers body timeout settle)
  "Begin sending BODY to URL with HEADERS; return the exchange.

SETTLE is called with the result plist, once, and not at all if
the exchange is abandoned first.  TIMEOUT, a positive number of
seconds, bounds the whole exchange; anything else waits for the
server or the network to end it.  Nothing here signals: a
request that cannot even be started settles as a connection
failure, and may do so before this returns."
  (let ((exchange (jev-http--make-exchange :settle settle))
        (buffer (generate-new-buffer " *jev-http*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    (setf (jev-http--exchange-buffer exchange) buffer)
    (condition-case err
        (let ((target (jev-http--target url)))
          (setf (jev-http--exchange-target exchange) target)
          (setf (jev-http--exchange-request exchange)
                (jev-http--request-text target headers body))
          (when (and (numberp timeout) (> timeout 0))
            (setf (jev-http--exchange-timer exchange)
                  (run-at-time timeout nil #'jev-http--settle exchange
                               (list :error 'timeout
                                     :message (format "No response after %ss" timeout)))))
          (setf (jev-http--exchange-process exchange)
                (jev-http--open target buffer
                                (lambda (_process data)
                                  (jev-http--on-output exchange data))
                                (lambda (process event)
                                  (jev-http--on-event exchange process event)))))
      (error
       (jev-http--settle exchange
                         (jev-http--connection-error
                          (format "Could not open a connection: %s"
                                  (error-message-string err))))))
    exchange))

(defun jev-http--socket-sync (url headers body timeout)
  "Send BODY to URL with HEADERS and wait for the result plist.
A \\[keyboard-quit] out of the wait takes the exchange down with
it, so nothing is left fetching for nobody."
  (let* ((result nil)
         (exchange (jev-http--start url headers body timeout
                                    (lambda (answer) (setq result answer)))))
    (unwind-protect
        (while (not (jev-http--exchange-done exchange))
          (accept-process-output nil 0.05))
      (jev-http--settle exchange nil))
    result))

(defun jev-http--socket (url headers body timeout sync callback)
  "Send BODY to URL with HEADERS over a socket of our own.
See `jev-http-function' for TIMEOUT, SYNC and CALLBACK.  An
asynchronous call returns a function that abandons the exchange."
  (if sync
      (jev-http--socket-sync url headers body timeout)
    (let ((exchange (jev-http--start url headers body timeout callback)))
      (lambda () (jev-http--settle exchange nil)))))


;;;; Retries

(defun jev-http--retryable-p (result)
  "Return non-nil when RESULT is worth retrying.
A transport failure is, and so are 408, 429 and 5xx."
  (let ((status (plist-get result :status)))
    (or (and (plist-get result :error) t)
        (and (integerp status)
             (or (memq status '(408 429)) (>= status 500))
             t))))

(defun jev-http--retry-after (result)
  "Return the Retry-After delay of RESULT in seconds, or nil."
  (when-let* ((value (alist-get "retry-after" (plist-get result :headers)
                                nil nil #'equal))
              (seconds (string-to-number (string-trim (format "%s" value)))))
    (and (> seconds 0) seconds)))

(defun jev-http--delay (attempt result)
  "Return how long to wait before ATTEMPT is retried, given RESULT."
  (min 60 (or (jev-http--retry-after result)
              (* jev-retry-initial-delay (expt 2 attempt)))))

(defun jev-http--send (url headers body timeout sync callback)
  "Perform one attempt, dispatching to the configured backend.
See `jev-http-function' for URL, HEADERS, BODY, TIMEOUT, SYNC and CALLBACK."
  (if jev-http-function
      (funcall jev-http-function url headers body timeout sync callback)
    (jev-http--socket url headers body timeout sync callback)))

(defun jev-http--send-sync (attempt url headers body)
  "Perform one synchronous ATTEMPT of BODY to URL with HEADERS.

A transport plugged in through `jev-http-function' can refuse a
request outright rather than answer it.  On the first attempt
that travels to the caller, which reports a request that was
never sent; from the second on there are retries left to spend
on it, so it becomes a connection failure like any other --
exactly as it does asynchronously."
  (condition-case caught
      (jev-http--send url headers body jev-timeout t nil)
    (error
     (when (zerop attempt) (signal (car caught) (cdr caught)))
     (jev-http--connection-error
      (format "Could not send the request: %s" (error-message-string caught))))))

(defun jev-http-request (url headers body sync callback &optional request)
  "Send BODY to URL with HEADERS, retrying when it makes sense.

When SYNC is non-nil, block and return the final result plist.
Otherwise return nil and call CALLBACK with it.  REQUEST, a
`jev-request', is told how to stop whatever is in flight, so that
`jev-cancel' reaches the attempt and any wait between attempts."
  (if sync
      (let ((attempt 0) result)
        (while (progn
                 (setq result (jev-http--send-sync attempt url headers body))
                 (and (jev-http--retryable-p result)
                      (< attempt (jev--max-retries))))
          (let ((delay (jev-http--delay attempt result)))
            (jev--log "retrying in %ss (attempt %d)" delay (1+ attempt))
            (sleep-for delay))
          (setq attempt (1+ attempt)))
        result)
    (jev-http--attempt 0 url headers body callback request)
    nil))

(defun jev-http--register-canceller (request canceller)
  "Let REQUEST stop the work CANCELLER stops, if REQUEST wants that.
Return non-nil when REQUEST was already cancelled, in which case
CANCELLER has been called instead of stored."
  (when (jev-request-p request)
    (if (jev-request-cancelled request)
        (progn (when canceller (funcall canceller)) t)
      (setf (jev-request-canceller request) canceller)
      nil)))

(defun jev-http--attempt (attempt url headers body callback &optional request)
  "Send one asynchronous ATTEMPT of BODY to URL with HEADERS, then CALLBACK.
REQUEST is as in `jev-http-request'.  A request cancelled before
this attempt starts is dropped here, and reports nothing."
  (unless (jev-cancelled-p request)
    (let ((settled nil) canceller handle)
      (setq handle
            (lambda (result)
              (setq settled t)
              ;; A cancelled request is not retried, but whatever came
              ;; back is still handed up: it was sent, it cost
              ;; something, and only the layer above knows whether
              ;; anyone still wants to hear about it.
              (if (and (not (jev-cancelled-p request))
                       (jev-http--retryable-p result)
                       (< attempt (jev--max-retries)))
                  (let* ((delay (jev-http--delay attempt result))
                         (timer (run-at-time delay nil #'jev-http--attempt
                                             (1+ attempt) url headers body
                                             callback request)))
                    (jev--log "retrying in %ss (attempt %d)" delay (1+ attempt))
                    ;; A request cancelled while waiting should not
                    ;; wake up and try again.
                    (jev-http--register-canceller
                     request (lambda () (cancel-timer timer))))
                (funcall callback result))))
      (setq canceller
            ;; A transport plugged in through `jev-http-function' may
            ;; refuse a request before it opens a connection.  On the
            ;; first attempt that signal travels out to `jev-ask',
            ;; which reports a request that was never sent.  A retry
            ;; has no caller left to signal to: it runs from a timer,
            ;; which swallows the signal, and the request would never
            ;; be heard from again -- no callback, no end hook.  So
            ;; from the second attempt on a refusal becomes a
            ;; connection failure like any other, retries included.
            (condition-case caught
                (jev-http--send url headers body jev-timeout nil handle)
              (error
               (when (zerop attempt)
                 (signal (car caught) (cdr caught)))
               (funcall handle
                        (jev-http--connection-error
                         (format "Could not send the request: %s"
                                 (error-message-string caught))))
               nil)))
      ;; Only if this attempt is still running.  A transport that
      ;; answers within the call above has already registered whatever
      ;; comes next -- the retry timer -- and overwriting that with a
      ;; canceller for an attempt that is over would leave
      ;; `jev-cancel' with nothing left to stop.
      (when (and (functionp canceller) (not settled))
        (jev-http--register-canceller request canceller)))))

(provide 'jev-http)
;;; jev-http.el ends here
