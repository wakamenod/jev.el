;;; jev.el --- Typed decisions from TypeSafe AI's Jev model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wakamenod

;; Author: wakamenod <wakamenod@gmail.com>
;; URL: https://github.com/wakamenod/jev.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Jev is TypeSafe AI's System One model.  It takes unstructured state
;; plus typed questions and answers each one with a typed value and a
;; calibrated confidence, in a single round trip.  This package is a
;; client for that API.
;;
;;   (setq jev-api-key "sk-...")          ; or TYPESAFE_API_KEY, or auth-source
;;
;;   (jev-ask "I was charged twice this month."
;;            (jev-questions
;;             (team   choice "Which team should handle this?"
;;                     '(("billing" . "Payments and invoices")
;;                       ("tech"    . "Bugs and outages")))
;;             (urgent noul   "Does this convey urgency?"))
;;            :success (lambda (reply _tag)
;;                       (message "%s (confidence %s), urgent: %s"
;;                                (jev-value reply 'team)
;;                                ;; nil where the provider sends none.
;;                                (or (jev-confidence reply 'team) "-")
;;                                (jev-true-p reply 'urgent))))
;;
;; `jev-ask' is asynchronous, which is what Emacs wants: the answer
;; arrives in a callback together with the TAG the call was made with,
;; so a caller can carry its own context across the round trip.
;; `jev-ask-sync' blocks and returns the reply, for batch jobs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jev-core)
(require 'jev-providers)
(require 'jev-http)
(require 'jev-usage)

;;;###autoload
(defmacro jev-questions (&rest specs)
  "Build a questions alist from SPECS.

Each SPEC is (KEY TYPE INSTRUCTIONS [CRITERIA]), where TYPE is one
of `noul', `choice' or `score'.  INSTRUCTIONS and CRITERIA are
evaluated, so they may be variables:

  (jev-questions
   (urgent   noul   \"Does this convey urgency?\")
   (team     choice \"Which team?\" \\='((\"billing\") (\"tech\")))
   (severity score  \"How severe is it?\" \\='(\"cosmetic\" \"annoying\" \"blocking\")))"
  (declare (indent 0))
  `(list
    ,@(mapcar
       (lambda (spec)
         (pcase-let ((`(,key ,type . ,args) spec))
           (let ((constructor (pcase type
                                ('noul #'jev-noul)
                                ('choice #'jev-choice)
                                ('score #'jev-score)
                                (_ (jev--signal
                                    'jev-invalid-question
                                    (format "Unknown Jev question type `%S'" type))))))
             `(cons ',key (,constructor ,@args)))))
       specs)))

;;;###autoload
(defun jev-use-provider (provider)
  "Set `jev-provider' to PROVIDER for the rest of the session.

Interactively, complete over the supported backends.
This changes the variable only; to make the choice permanent put
\\='(setq jev-provider \\='vercel)\\=' in your init file or use
\\[customize-variable]."
  (interactive
   (list (intern (completing-read
                  (format "Jev provider (now %s): " jev-provider)
                  (mapcar (lambda (cell) (symbol-name (car cell))) jev--providers)
                  nil t))))
  (jev--provider provider)
  (setq jev-provider provider)
  (when (called-interactively-p 'interactive)
    (message "Jev now talks to %s" provider))
  provider)

(defconst jev--request-id-headers
  '("x-typesafe-request-id" "request-id" "x-request-id" "x-vercel-id")
  "Response headers that may carry a correlation id, most specific first.")

(defun jev--request-id (result)
  "Return the request id carried by the HTTP RESULT, if any."
  (let ((headers (plist-get result :headers)))
    (cl-some (lambda (name) (alist-get name headers nil nil #'equal))
             jev--request-id-headers)))

(defun jev--payload (state questions model &optional provider)
  "Return the request body for STATE, QUESTIONS and MODEL.
PROVIDER is a provider plist, defaulting to the current one; the
shape is that provider\\='s, not one common format."
  (funcall (plist-get (or provider (jev--provider)) :body) state questions model))

(defun jev--error-symbol (status)
  "Return the error symbol matching the HTTP STATUS."
  (pcase status
    (401 'jev-auth-error)
    (403 'jev-auth-error)
    (402 'jev-billing-error)
    (422 'jev-validation-error)
    (429 'jev-rate-limit-error)
    (529 'jev-overloaded-error)
    (_ 'jev-api-error)))

(defun jev--error-entry (entry)
  "Return ENTRY of an error array as a message, or nil when it says nothing.
An entry with neither a message nor a code would read as the
word \"nil\", which tells the caller less than the HTTP status it
would have displaced."
  (let ((message (alist-get 'message entry))
        (code (alist-get 'code entry)))
    (cond ((and message code) (format "%s (code %s)" message code))
          (message (format "%s" message))
          (code (format "Error code %s" code)))))

(defun jev--error-detail (body)
  "Return the message carried by the JSON error BODY, or nil.
Providers disagree on where to put it: a bare `message', an
`error' or `detail' that is a string or an object with a
`message' in it, or an `errors' array."
  (ignore-errors
    (let ((decoded (json-parse-string body :object-type 'alist
                                      :null-object nil :false-object nil)))
      (or (alist-get 'message decoded)
          (cl-some (lambda (field)
                     (let ((err (alist-get field decoded)))
                       (cond ((stringp err) err)
                             ((consp err) (alist-get 'message err)))))
                   '(error detail))
          (let ((parts (delq nil (mapcar #'jev--error-entry
                                         (append (alist-get 'errors decoded) nil)))))
            (when parts (mapconcat #'identity parts "; ")))))))

(defun jev--handle-result (result questions &optional provider model)
  "Return the `jev-reply' decoded from the HTTP RESULT, or signal.
QUESTIONS is the request it answers and MODEL the model it was
sent for, both of which some providers need to fill in what they
leave out of the response.  PROVIDER is the backend that
answered, which is not necessarily the one `jev-provider' names
by now."
  (let ((status (plist-get result :status))
        (request-id (jev--request-id result)))
    (cond
     ((eq (plist-get result :error) 'timeout)
      (jev--signal 'jev-timeout-error (plist-get result :message)))
     ((plist-get result :error)
      (jev--signal 'jev-connection-error (plist-get result :message)))
     ((and status (<= 200 status 299))
      (funcall (plist-get (jev--provider provider) :parse)
               (jev--decode-json (plist-get result :body))
               request-id questions model))
     ;; The endpoint answered, with somewhere else to ask.  The API
     ;; key travels in the request headers, so the transport follows
     ;; nobody anywhere; and this is not a connection failure, because
     ;; a caller that retries those would only be redirected again.
     ((and status (<= 300 status 399))
      (jev--signal 'jev-api-error
                   (format "The Jev API redirected the request to %s, which is not followed"
                           (or (alist-get "location" (plist-get result :headers)
                                          nil nil #'equal)
                               "somewhere else"))
                   :status status :request-id request-id
                   :body (plist-get result :body)))
     (t
      (let ((body (plist-get result :body)))
        (jev--signal (jev--error-symbol status)
                     (or (jev--error-detail body)
                         (format "Jev API returned HTTP %s" status))
                     :status status :request-id request-id :body body))))))

(defun jev--later (request thunk)
  "Call THUNK on a later turn, unless REQUEST has been cancelled by then.

Every callback of `jev-ask' goes through here, so that none of
them ever runs before `jev-ask' has returned -- whatever the
transport did -- and none of them runs inside a process filter.
An error THUNK raises is the caller's own: it is reported here,
the same way on every Emacs, and enters the debugger when
`debug-on-error' asks for one."
  (run-at-time 0 nil (lambda ()
                       (unless (jev-cancelled-p request)
                         (condition-case-unless-debug err
                             (funcall thunk)
                           (error (message "jev: callback signalled: %s"
                                           (error-message-string err))))))))

(defun jev--read-result (result questions provider model)
  "Return (REPLY . ERR) for the HTTP RESULT, one of them nil.
QUESTIONS, PROVIDER and MODEL are as in `jev--handle-result'.
Nothing escapes: a reply that cannot be read is an error object
like any other, never a raw signal into whoever is calling."
  (condition-case caught
      (cons (jev--handle-result result questions provider model) nil)
    (jev-error (cons nil caught))
    (error (cons nil (jev--unreadable-reply caught result)))))

(defun jev--prepare (state questions model)
  "Return (URL HEADERS BODY NAME MODEL) for STATE, QUESTIONS and MODEL.
Everything provider-specific is decided here, by `jev-provider'.
NAME is the provider the request was prepared for, and travels
with it because `jev-provider' may well be something else by the
time the reply lands; so does the model that was settled on.

This only builds the request.  The start hooks are run by
`jev--start', once the caller is in a position to answer for them
with a matching end hook."
  (let* ((name jev-provider)
         (provider (jev--provider name))
         (model (or model (jev--provider-model name)))
         (url (funcall (plist-get provider :url) model))
         (headers (funcall (plist-get provider :headers) model))
         (body (jev--payload state questions model provider)))
    (jev--log "POST %s %s" url body)
    (list url headers body name model)))

(defun jev--start (url provider model questions)
  "Run the start hooks for the request about to go to URL.
PROVIDER, MODEL and QUESTIONS describe it.  A hook that signals
takes the request down with it, so this is called from where the
end hooks can still be balanced."
  (run-hook-with-args 'jev-request-start-functions
                      (list :url url :provider provider
                            :model model :questions questions)))

(defun jev--run-end-hooks (info)
  "Call every function on `jev-request-end-functions' with INFO.

Isolated one from the next, unlike the start hooks, which are
allowed to take a request down before it leaves.  By the time
these run there is a reply to deliver and a caller waiting for
it, and the session totals are counted from this same hook: one
observer that signals must not silence the others, lose the
answer, or escape into a timer where nobody is listening."
  (run-hook-wrapped
   'jev-request-end-functions
   (lambda (function info)
     (condition-case err
         (funcall function info)
       (error (jev--log "end hook %S signalled: %s"
                        function (error-message-string err))
              (message "jev: request end hook %S signalled: %s"
                       function (error-message-string err))))
     ;; Never stop early: `run-hook-wrapped' takes non-nil for done.
     nil)
   info))

(defun jev--finish (url provider started result reply err)
  "Run the end hooks for the request to URL that STARTED at that time.
PROVIDER is the backend it was sent to, RESULT the raw HTTP
result, REPLY the decoded reply and ERR the error object, any of
which may be nil."
  (jev--log "<- %s %s" (or (plist-get result :status) (plist-get result :error))
            (or (plist-get result :body) ""))
  (jev--run-end-hooks
   (list :url url
         :provider provider
         :status (plist-get result :status)
         :duration (float-time (time-since started))
         :usage (and reply (jev-reply-usage reply))
         :input-tokens (and reply (jev-input-tokens reply))
         :output-tokens (and reply (jev-output-tokens reply))
         :cost (and reply (jev-cost reply))
         :request-id (or (and reply (jev-reply-request-id reply))
                         (jev--request-id result))
         :error err)))

(defun jev--unreadable-reply (caught result)
  "Return the error object for a reply CAUGHT could not read, given RESULT."
  (list 'jev-response-error
        (format "Could not read the reply: %s" (error-message-string caught))
        :body (plist-get result :body)))

(defun jev--unsendable (caught)
  "Return the error object for a request the transport refused with CAUGHT."
  (list 'jev-connection-error
        (format "Could not send the request: %s" (error-message-string caught))))

(defun jev--interrupted ()
  "Return the error object for a request \\[keyboard-quit] abandoned.

A quit is the caller walking away from a request that was already
sent, which is what `jev-cancel' is, so the end hooks hear the
same thing they hear from it."
  (list 'jev-cancelled "Request interrupted"))

(defun jev--unbuildable (caught)
  "Return the error object for a request CAUGHT stopped from being built.

Not a connection failure: nothing was sent, and no amount of
waiting and trying again would help.  A state holding a NaN is
`json-serialize' refusing it, in a plain `error' -- and a caller
that retries every connection error must not be sent round that
loop forever by a state it can never encode."
  (list 'jev-invalid-state
        (format "Could not build the request: %s" (error-message-string caught))))

;;;###autoload
(cl-defun jev-ask (state questions &key success ((:error errback)) tag model)
  "Ask QUESTIONS about STATE, asynchronously.

STATE is the material to judge: a string, or any JSON-encodable
Lisp value (an alist, plist, hash table or vector).  QUESTIONS is
an alist of (KEY . QUESTION), most easily written with
`jev-questions'.

On success SUCCESS is called with the `jev-reply' and TAG.  On
failure ERRBACK, given as the :error keyword, is called with the
error object and TAG; without one the error is reported with
`message'.  TAG is opaque to this package and is how a caller
carries context across the round trip.  MODEL overrides the
default model of the current `jev-provider'.

Once called, this function fails only through ERRBACK: not a
missing key, not a question that is not one, not a state that
cannot be encoded, and not a transport that refuses the request
before it opens a connection.  Every one of them reaches ERRBACK,
and never before this function has returned.

A question that is malformed is a different matter, because
`jev-noul', `jev-choice' and `jev-score' check it where it is
written: an option list with one option in it signals
`jev-invalid-question' from the caller\\='s own frame, before this
function is ever reached.  That is deliberate -- a rubric is
usually a constant, and a constant is better wrong at the call
site than one round trip later -- but it does mean QUESTIONS has
to be built somewhere the caller is prepared for it.

Neither callback ever runs before this function has returned,
whatever the transport did, so a caller may keep the request
this returns and read it from inside either.  An error raised by
SUCCESS or ERRBACK themselves is the caller's own: it is reported
with `message', and the request is unaffected.  A
\\[keyboard-quit] pressed while the request is being sent is the
caller's too: it is not reported again through ERRBACK, and the
request is abandoned as `jev-cancel' abandons one.

Return a `jev-request', which `jev-cancel' abandons: after that
neither callback runs, and the end hooks report an error of
`cancelled'.  A command that answers into a buffer wants this
when the buffer is killed, or when the user asks again before
the first answer lands."
  (let* ((request (jev--make-request :tag tag))
         (deliver
          ;; The one way out to the caller, for a reply and for
          ;; every kind of failure alike -- and always later.
          (lambda (reply err)
            (jev--later request
                        (lambda ()
                          (cond (err (if errback
                                         (funcall errback err tag)
                                       (message "jev: %s" (jev-error-message err))))
                                (success (funcall success reply tag)))))))
         (prepared (condition-case caught
                       (cons 'ready (jev--prepare state questions model))
                     (jev-error (cons 'failed caught))
                     ;; Not every way of failing to build a request is
                     ;; one of ours: `json-serialize' refuses a state
                     ;; holding a NaN, and it says so in a plain
                     ;; `error'.  Letting that one through would make
                     ;; this function throw for some bad states and
                     ;; call back for others.
                     (error (cons 'failed (jev--unbuildable caught))))))
    ;; A key that is not configured, or a malformed question, is found
    ;; before anything is sent.  It goes to ERRBACK like every other
    ;; failure: nothing was sent and no start hook has run, so the end
    ;; hooks hear nothing either.
    (if (eq (car prepared) 'failed)
        (funcall deliver nil (cdr prepared))
      (pcase-let* ((`(,url ,headers ,body ,provider ,sent-model) (cdr prepared))
                   (started (current-time))
                   (finished nil)
                   (finish nil))
        ;; The end hooks hear about this request exactly once,
        ;; whichever way it ends: a reply, a refusal, or a
        ;; cancellation that stopped it before either.
        (setq finish
              (lambda (result reply err)
                (unless finished
                  (setq finished t)
                  ;; Nothing of this request is still running, so
                  ;; `jev-cancel' has nothing left to stop and must
                  ;; not reach into an attempt that is over.
                  (setf (jev-request-on-cancel request) nil)
                  (setf (jev-request-canceller request) nil)
                  (jev--finish url provider started result reply err))))
        ;; An abandoned request was still sent and still cost
        ;; something, so it is reported as soon as it is stopped.
        ;; Where nothing could be stopped, a reply may yet land, and
        ;; the callback below reports it with its usage instead.
        (setf (jev-request-on-cancel request)
              (lambda (stopped)
                (when stopped
                  (funcall finish nil nil (list 'jev-cancelled "Request cancelled")))))
        (condition-case caught
            (progn
              ;; Inside the handler, because a start hook is someone
              ;; else's code and may signal.  Some of them have run by
              ;; then, so that request has to reach the end hooks too.
              (jev--start url provider sent-model questions)
              (jev-http-request
               url headers body nil
               (lambda (result)
                 ;; A transport answering twice is one not keeping its
                 ;; side of the bargain, and `jev-http-function' is
                 ;; someone else's code.  The first answer is the
                 ;; request's; the second is dropped.
                 (unless finished
                   (pcase-let ((`(,reply . ,err)
                                (jev--read-result result questions provider sent-model)))
                     (if (jev-cancelled-p request)
                         ;; Nobody is waiting for this any more, but
                         ;; it was still sent and still cost something.
                         (funcall finish result reply
                                  (list 'jev-cancelled "Request cancelled"))
                       (funcall finish result reply err)
                       (funcall deliver reply err)))))
               request))
          ;; Nothing of the caller's runs inside the call above, so
          ;; whatever it signals is the request failing to leave: a
          ;; start hook, or a transport refusing it before it opened
          ;; a connection.  That belongs in ERRBACK, and the end hooks
          ;; still have to balance the start hooks that have run.
          (jev-error
           (funcall finish nil nil caught)
           (funcall deliver nil caught))
          (error
           (let ((err (jev--unsendable caught)))
             (funcall finish nil nil err)
             (funcall deliver nil err)))
          ;; A quit is not an `error', so without this it would travel
          ;; out of here leaving the start hooks unbalanced, an
          ;; attempt still in flight, and callbacks about to run for a
          ;; caller who has already pressed C-g.  The request is
          ;; abandoned exactly as `jev-cancel' abandons one -- no
          ;; callbacks, `cancelled' to the end hooks -- and the quit
          ;; travels on.
          (quit
           (let ((canceller (jev-request-canceller request)))
             (setf (jev-request-cancelled request) t)
             (funcall finish nil nil (jev--interrupted))
             (when canceller (funcall canceller)))
           (signal (car caught) (cdr caught))))))
    request))

;;;###autoload
(cl-defun jev-ask-sync (state questions &key tag model)
  "Ask QUESTIONS about STATE and block until the reply arrives.

Return a `jev-reply', or signal a `jev-error'.  STATE, QUESTIONS,
TAG and MODEL are as in `jev-ask'; TAG is accepted only so that
the two calls read alike, and is ignored."
  (ignore tag)
  (pcase-let* ((`(,url ,headers ,body ,provider ,sent-model)
                ;; Building the request can fail in ways that are not
                ;; ours -- `json-serialize' refuses a state holding a
                ;; NaN -- and this function promises a `jev-error'
                ;; whatever went wrong.  Nothing was sent and no start
                ;; hook has run, so the end hooks hear nothing.
                (condition-case caught
                    (jev--prepare state questions model)
                  (jev-error (signal (car caught) (cdr caught)))
                  (error (let ((err (jev--unbuildable caught)))
                           (signal (car err) (cdr err))))))
               (started (current-time))
               (result nil))
    ;; Both the sending and the reading can fail, and either way the
    ;; caller is owed a `jev-error' and the end hooks a matching call.
    ;; The start hooks run in here too: one of them signalling is a
    ;; failure like any other, and some of them have run by then.
    (condition-case caught
        (progn
          (jev--start url provider sent-model questions)
          (setq result (jev-http-request url headers body t nil)))
      (jev-error (jev--finish url provider started nil nil caught)
                 (signal (car caught) (cdr caught)))
      (error (let ((err (jev--unsendable caught)))
               (jev--finish url provider started nil nil err)
               (signal (car err) (cdr err))))
      ;; C-g out of the wait is not an `error' and would leave the
      ;; start hooks above with nothing to balance them, which is the
      ;; one thing the end hooks promise never to do.  The request
      ;; ends here the way an abandoned one does, and the quit goes on
      ;; its way to whoever pressed the key.
      (quit (jev--finish url provider started nil nil (jev--interrupted))
            (signal (car caught) (cdr caught))))
    (pcase-let ((`(,reply . ,err)
                 (condition-case caught
                     (jev--read-result result questions provider sent-model)
                   (quit (jev--finish url provider started result nil (jev--interrupted))
                         (signal (car caught) (cdr caught))))))
      (jev--finish url provider started result reply err)
      (when err (signal (car err) (cdr err)))
      reply)))

(provide 'jev)
;;; jev.el ends here
