;;; jev-core.el --- Core types, questions and encoding for Jev -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wakamenod

;; Author: wakamenod <wakamenod@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Jev is TypeSafe AI's "System One" model: it takes unstructured state
;; plus typed questions and returns typed answers -- a noul (yes/no), a
;; choice or a score -- each with a calibrated confidence.
;;
;; This file holds everything that does not touch the network:
;; customization, the error hierarchy, question constructors, JSON
;; normalisation and the reply objects.  See `jev.el' for the public
;; entry points `jev-ask' and `jev-ask-sync'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'auth-source)
(require 'url-parse)

;; Emacs 27 and 28 only have these when they were built against
;; libjansson, and a package that fails later is worse than one that
;; says so on load.
(unless (and (fboundp 'json-serialize) (fboundp 'json-parse-string))
  (error "Jev needs an Emacs built with native JSON support (libjansson)"))

(defgroup jev nil
  "Client for TypeSafe AI's Jev System One model."
  :group 'tools
  :prefix "jev-")

(defcustom jev-api-key nil
  "API key used to authenticate against the Jev API.

One of:

  nil         look the key up in the provider's environment
              variable, then in auth-source by endpoint host;
  a string    that key, whichever provider is in use;
  a function  called with the provider symbol, returning the key;
  an alist    keyed by provider symbol, each value a string or a
              function, so several providers can be configured at
              once and `jev-provider' picks between them:

    (setq jev-api-key \\='((vercel . \"vck_...\")
                        (typesafe . my-typesafe-key-function)))

Prefer auth-source to a literal key in your init file."
  :type '(choice (const :tag "Environment / auth-source" nil)
                 (string :tag "Literal key")
                 (function :tag "Function of the provider symbol")
                 (alist :tag "Per provider" :key-type symbol)))

(defcustom jev-base-url "https://api.typesafe.ai"
  "Base URL of the Jev API, without a trailing slash."
  :type 'string)

(defcustom jev-default-model "jev-latest"
  "Model used when a request does not specify one."
  :type 'string)

(defcustom jev-timeout 30
  "Timeout in seconds for a single HTTP attempt.

Set to nil to wait indefinitely.  Nothing then ends a request but
the server, the network or `jev-cancel', so keep a handle on an
asynchronous one."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Wait indefinitely" nil)))

(defcustom jev-max-retries 2
  "How many times a retryable request is retried.

Connection failures, timeouts and HTTP 408, 429 and 5xx responses
are retryable.  Zero, or anything that is not a whole number,
means the one attempt and no more."
  :type '(choice (integer :tag "Retries")
                 (const :tag "No retries" nil)))

(defun jev--max-retries ()
  "Return how many retries to make, whatever `jev-max-retries' holds.
A setting that is not a whole number cannot be counted down
against, and there is nothing useful to do with it but send the
request once."
  (if (integerp jev-max-retries) (max 0 jev-max-retries) 0))

(defcustom jev-retry-initial-delay 0.5
  "Base delay in seconds for the exponential retry backoff."
  :type 'number)

(defcustom jev-input-token-price 0.042
  "Price in US dollars per million input tokens.

Only used to estimate the cost of a reply whose provider reports
none; output tokens are not billed.  Jev listed at $0.042 per
million input tokens in September 2026."
  :type 'number)

(defcustom jev-log nil
  "When non-nil, log requests and responses to `jev-log-buffer-name'."
  :type 'boolean)

(defcustom jev-log-buffer-name "*jev-log*"
  "Name of the buffer used when `jev-log' is non-nil."
  :type 'string)

;; A file-local variable must not be able to hand the key to someone
;; else: not by replacing it, and not by pointing the endpoint at
;; another host.  Marking these risky makes Emacs ask first.
;; `jev-log' is here too: it writes the state of every request into a
;; buffer a file-local could name, which is someone else's data going
;; somewhere else's buffer.
(dolist (symbol '(jev-api-key jev-auth-source-user jev-base-url
                  jev-log jev-log-buffer-name))
  (put symbol 'risky-local-variable t))

(defvar jev-request-start-functions nil
  "Functions called when a request is about to be sent.
Each is called with a plist holding :url, :provider, :model and
:questions.")

(defvar jev-request-end-functions nil
  "Functions called when a request finished, successfully or not.
Each is called with a plist holding :url, :provider, :status,
:duration, :usage, :input-tokens, :output-tokens, :cost,
:request-id and :error.")


;;;; Errors

(define-error 'jev-error "Jev error")
(define-error 'jev-configuration-error "Jev is not configured" 'jev-error)
(define-error 'jev-invalid-question "Invalid Jev question" 'jev-error)
(define-error 'jev-invalid-state "Invalid Jev state" 'jev-error)
(define-error 'jev-response-error "Malformed Jev response" 'jev-error)
(define-error 'jev-cancelled "Jev request cancelled" 'jev-error)
(define-error 'jev-connection-error "Jev connection failed" 'jev-error)
(define-error 'jev-timeout-error "Jev request timed out" 'jev-connection-error)
(define-error 'jev-api-error "Jev API error" 'jev-error)
(define-error 'jev-auth-error "Jev authentication failed" 'jev-api-error)
(define-error 'jev-validation-error "Jev rejected the request" 'jev-api-error)
(define-error 'jev-billing-error "Jev provider needs payment" 'jev-api-error)
(define-error 'jev-rate-limit-error "Jev rate limit exceeded" 'jev-api-error)
(define-error 'jev-overloaded-error "Jev is overloaded" 'jev-api-error)

(defun jev--signal (symbol message &rest props)
  "Signal SYMBOL with MESSAGE and a property list PROPS."
  (signal symbol (cons message props)))

(defun jev-error-message (err)
  "Return the human readable message of the error object ERR."
  (cadr err))

(defun jev-error-status (err)
  "Return the HTTP status carried by the error object ERR, if any."
  (plist-get (cddr err) :status))

(defun jev-error-request-id (err)
  "Return the request id carried by the error object ERR, if any."
  (plist-get (cddr err) :request-id))

(defun jev-error-body (err)
  "Return the raw response body carried by the error object ERR, if any."
  (plist-get (cddr err) :body))


;;;; Configuration helpers

(defcustom jev-auth-source-user nil
  "Login to match when looking a key up in auth-source.

Left nil, any entry for the endpoint host is accepted.  Set it
when one host holds several secrets, so that this one is found:

  machine ai-gateway.vercel.sh login jev password vck_..."
  :type '(choice (const :tag "Any entry for the host" nil) string))

(defun jev--getenv (name)
  "Return the environment variable NAME, treating an empty value as unset."
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun jev--auth-source-key (&optional url)
  "Look up an API key for the host of URL in auth-source.
URL defaults to `jev-base-url'.

Only the host is matched, never the port or the protocol, so any
entry for that host will do; `jev-auth-source-user' is the way to
narrow it when one host holds several secrets."
  (when-let* ((host (url-host (url-generic-parse-url (or url jev-base-url))))
              (found (car (apply #'auth-source-search
                                 :host host :max 1 :require '(:secret)
                                 (when jev-auth-source-user
                                   (list :user jev-auth-source-user)))))
              (secret (plist-get found :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun jev--configured-key (provider)
  "Return the key `jev-api-key' holds for PROVIDER, or nil."
  (let ((value (if (and (consp jev-api-key) (consp (car jev-api-key)))
                   (alist-get provider jev-api-key)
                 jev-api-key)))
    (cond ((functionp value) (funcall value provider))
          ((stringp value) value))))

(defun jev--api-key (provider &optional env-var url)
  "Resolve the API key for PROVIDER, or signal `jev-configuration-error'.

`jev-api-key' is consulted first; failing that ENV-VAR is read
from the environment and the host of URL looked up in
auth-source."
  (let* ((env-var (or env-var "TYPESAFE_API_KEY"))
         (key (or (jev--configured-key provider)
                  (jev--getenv env-var)
                  (jev--auth-source-key url))))
    (unless (and (stringp key) (not (string-empty-p key)))
      (jev--signal 'jev-configuration-error
                   (format "No Jev API key for `%s': set `jev-api-key' or %s"
                           provider env-var)))
    key))

(defun jev--endpoint (path)
  "Return the absolute URL for PATH under `jev-base-url'."
  (concat (string-remove-suffix "/" jev-base-url) path))

(defun jev--log (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to the log buffer."
  (when jev-log
    (with-current-buffer (get-buffer-create jev-log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S.%3N] ")
              (apply #'format format-string args)
              "\n"))))


;;;; JSON normalisation
;;
;; `json-serialize' cannot tell an empty alist from nil from an empty
;; list, and it encodes Lisp lists as objects, never as arrays.  Every
;; value handed to it therefore goes through `jev--json' first, which
;; maps Lisp data onto an unambiguous representation:
;;
;;   nil          -> null      (use `jev-empty-object' for `{}')
;;   t            -> true      :false -> false   :null -> null
;;   alist/plist  -> object    hash-table -> object
;;   other list   -> array     vector -> array
;;   symbol       -> string    :keyword -> "keyword", as for a key

(defconst jev-empty-object (make-hash-table :test #'equal)
  "Value encoding an empty JSON object.

Recognised by identity, so that something written into it by
accident cannot turn every `{}' in the session into an object
with a key in it.")

(defun jev--plist-p (list)
  "Return non-nil when LIST looks like a property list."
  (and (consp list) (keywordp (car list))))

(defun jev--alist-p (list)
  "Return non-nil when LIST looks like an association list."
  (and (consp list) (consp (car list))))

(defun jev--field (alist key)
  "Return KEY of ALIST, or nil unless ALIST really is one.

Decoding a response reads a lot of fields a provider is free to
send in some other shape.  Where an object was expected and an
array arrived, that field is worth nothing in particular to the
caller -- which is not the same as taking the request down with a
`wrong-type-argument'."
  (and (jev--alist-p alist) (alist-get key alist)))

(defun jev--key-name (key)
  "Return KEY as a JSON object key string."
  (cond ((stringp key) key)
        ((keywordp key) (substring (symbol-name key) 1))
        ((symbolp key) (symbol-name key))
        (t (format "%s" key))))

(defun jev--json (value)
  "Return VALUE in a form `json-serialize' encodes unambiguously."
  (cond
   ((null value) :null)
   ((eq value t) t)
   ((memq value '(:null :false)) value)
   ((stringp value) value)
   ((numberp value) value)
   ;; Before the general hash table case, and a fresh one every time:
   ;; whatever the constant holds by now, it stands for `{}'.
   ((eq value jev-empty-object) (make-hash-table :test #'equal))
   ((hash-table-p value)
    (let ((out (make-hash-table :test #'equal :size (hash-table-count value))))
      (maphash (lambda (k v) (puthash (jev--key-name k) (jev--json v) out)) value)
      out))
   ((vectorp value) (vconcat (mapcar #'jev--json value)))
   ((jev--plist-p value)
    (let ((out (make-hash-table :test #'equal)))
      (cl-loop for (k v) on value by #'cddr
               do (puthash (jev--key-name k) (jev--json v) out))
      out))
   ((jev--alist-p value)
    (let ((out (make-hash-table :test #'equal)))
      (dolist (cell value out)
        (puthash (jev--key-name (car cell)) (jev--json (cdr cell)) out))))
   ((listp value) (vconcat (mapcar #'jev--json value)))
   ;; `jev--key-name' strips the colon from a keyword used as a key;
   ;; a keyword used as a value reads the same way.
   ((symbolp value) (jev--key-name value))
   (t (format "%s" value))))


;;;; Questions

(cl-defstruct (jev-question (:constructor jev--make-question) (:copier nil))
  "A single typed question.
TYPE is one of the symbols `noul', `choice' or `score'."
  type instructions criteria)

(defun jev--check-instructions (instructions)
  "Signal unless INSTRUCTIONS is a non-empty string."
  (unless (and (stringp instructions) (not (string-empty-p (string-trim instructions))))
    (jev--signal 'jev-invalid-question
                 (format "Question instructions must be a non-empty string, got %S"
                         instructions))))

(defun jev--label (label)
  "Return LABEL as a criteria key string, or signal.

A keyword loses its colon, exactly as `jev--key-name' drops it
from an object key: `:billing' and `billing' name one option,
not two.  A number is spelled out, so that a rubric written
`(1 2 3 4 5)' is the rubric it looks like.

A label with nothing in it is refused, as empty instructions
are: it reaches the model as an option it cannot tell apart from
the absence of one, and comes back from `jev-level' as a level
with no name."
  (let ((name (cond ((stringp label) label)
                    ((or (symbolp label) (numberp label)) (jev--key-name label))
                    (t (jev--signal
                        'jev-invalid-question
                        (format "Criteria label must be a string, symbol or number, got %S"
                                label))))))
    (when (string-empty-p (string-trim name))
      (jev--signal 'jev-invalid-question
                   (format "Criteria label must not be empty, got %S" label)))
    name))

(defun jev--description (description)
  "Return DESCRIPTION as a criteria description string, or nil, or signal.

A one-element list is what `(LABEL DESCRIPTION)' leaves behind,
and means the same as the pair."
  (cond ((null description) nil)
        ((stringp description) description)
        ((and (consp description) (stringp (car description)) (null (cdr description)))
         (car description))
        (t (jev--signal
            'jev-invalid-question
            (format "Criteria description must be a string, got %S" description)))))

(defun jev--normalize-labelled-criteria (criteria)
  "Normalise CRITERIA into an alist of (LABEL . DESCRIPTION-OR-NIL).

CRITERIA accepts a list of labels, an alist of (LABEL . DESCRIPTION)
or (LABEL DESCRIPTION) pairs, or a hash table.  Whichever way it
is written, the labels and the descriptions are checked the
same: a hash table is another spelling of the same options, not
a way past the checks and onto the wire."
  (cond
   ((hash-table-p criteria)
    (let (out)
      (maphash (lambda (k v) (push (cons (jev--label k) (jev--description v)) out))
               criteria)
      (nreverse out)))
   ((listp criteria)
    (mapcar (lambda (entry)
              (if (consp entry)
                  (cons (jev--label (car entry)) (jev--description (cdr entry)))
                (cons (jev--label entry) nil)))
            criteria))
   (t (jev--signal 'jev-invalid-question
                   (format "Criteria must be a list or hash table, got %S" criteria)))))

(defun jev--check-distinct (labels message)
  "Signal MESSAGE as `jev-invalid-question' unless LABELS are all different.

Two options under one label reach the model as one option, and an
answer naming it says nothing about which of them was meant."
  (unless (= (length labels) (length (delete-dups (copy-sequence labels))))
    (jev--signal 'jev-invalid-question message)))

(defconst jev--noul-labels '("true" "false")
  "The only two things a noul's criteria can describe.")

;;;###autoload
(defun jev-noul (instructions &optional criteria)
  "Return a yes/no question asking INSTRUCTIONS.

CRITERIA optionally describes what true and false mean, as an
alist such as \\='((\"true\" . \"...\") (\"false\" . \"...\")).
The answer is the probability, between 0 and 1, that the
statement holds, so those two labels are the only ones there are:
a description filed under anything else would never be read.
Either may be given on its own."
  (jev--check-instructions instructions)
  (let ((normalized (and criteria (jev--normalize-labelled-criteria criteria))))
    (jev--check-distinct (mapcar #'car normalized)
                         "A noul describes the same side twice")
    (dolist (cell normalized)
      (unless (member (car cell) jev--noul-labels)
        (jev--signal 'jev-invalid-question
                     (format "A noul is answered true or false, so it has no `%s' to describe"
                             (car cell)))))
    (jev--make-question
     :type 'noul :instructions instructions :criteria normalized)))

;;;###autoload
(defun jev-choice (instructions criteria)
  "Return a question asking INSTRUCTIONS, answered by one of CRITERIA.

CRITERIA holds between 2 and 255 options; their order carries no
meaning.  Each option may be a bare label or a (LABEL . DESCRIPTION)
pair, and a description makes the option much easier to pick well."
  (jev--check-instructions instructions)
  (let* ((normalized (jev--normalize-labelled-criteria criteria))
         (labels (mapcar #'car normalized)))
    (unless (<= 2 (length labels) 255)
      (jev--signal 'jev-invalid-question
                   (format "A choice needs between 2 and 255 options, got %d"
                           (length labels))))
    (jev--check-distinct labels "A choice has duplicate options")
    (jev--make-question :type 'choice :instructions instructions :criteria normalized)))

;;;###autoload
(defun jev-score (instructions criteria)
  "Return a question asking INSTRUCTIONS, answered on the CRITERIA rubric.

CRITERIA is an ordered list or vector of between 2 and 10 levels,
lowest first; unlike a choice, that order is the rubric.  A level
may be written as a string, a symbol or a number, so a rubric of
\\='(1 2 3 4 5) has five rungs, named for the numbers themselves.

The answer is a weighted position on that rubric, not a 0-1
rating: with four levels it lands somewhere in 0.0-3.0.  Use
`jev-level' to turn it back into a label."
  (jev--check-instructions instructions)
  ;; A choice takes a hash table, because its options are unordered
  ;; and a hash table says so.  Here the order is the whole point, so
  ;; it has to come from something that has one.
  (unless (or (listp criteria) (vectorp criteria))
    (jev--signal 'jev-invalid-question
                 (format "A score rubric is ordered, so it needs a list or a vector, got %S"
                         criteria)))
  (let ((levels (mapcar #'jev--label (append criteria nil))))
    (unless (<= 2 (length levels) 10)
      (jev--signal 'jev-invalid-question
                   (format "A score needs between 2 and 10 levels, got %d"
                           (length levels))))
    ;; Two levels spelled alike are one rung of the rubric written
    ;; twice: `jev-level' would name it whichever way the score fell.
    (jev--check-distinct levels "A score has duplicate levels")
    (jev--make-question :type 'score :instructions instructions :criteria levels)))

(defun jev--question-json (question)
  "Return QUESTION as a JSON-encodable hash table."
  (unless (jev-question-p question)
    (jev--signal 'jev-invalid-question
                 (format "Not a Jev question: %S; use `jev-noul', `jev-choice' or `jev-score'"
                         question)))
  (let ((out (make-hash-table :test #'equal))
        (criteria (jev-question-criteria question)))
    (puthash "type" (symbol-name (jev-question-type question)) out)
    (puthash "instructions" (jev-question-instructions question) out)
    (pcase (jev-question-type question)
      ('score (puthash "criteria" (vconcat criteria) out))
      (_ (when criteria
           (let ((obj (make-hash-table :test #'equal)))
             (dolist (cell criteria)
               (puthash (car cell) (if (cdr cell) (cdr cell) :null) obj))
             (puthash "criteria" obj out)))))
    out))

(defun jev--questions-json (questions &optional encode)
  "Return the QUESTIONS alist as a JSON-encodable hash table.

ENCODE turns one question into its JSON form and defaults to
`jev--question-json'; a provider that spells a question its own
way passes its own, and gets the same checks with it."
  (unless (and (consp questions) (jev--alist-p questions))
    (jev--signal 'jev-invalid-question
                 (format "Questions must be an alist of (KEY . QUESTION), got %S" questions)))
  (let ((out (make-hash-table :test #'equal))
        (encode (or encode #'jev--question-json)))
    (dolist (cell questions out)
      (let ((name (jev--key-name (car cell))))
        ;; Two questions under one key travel as one, and the answer
        ;; comes back for whichever of them was sent -- silently, and
        ;; for the other one's caller too.
        (when (gethash name out)
          (jev--signal 'jev-invalid-question
                       (format "Two questions share the key %s" name)))
        (puthash name (funcall encode (cdr cell)) out)))))

(defun jev--escape-non-ascii (string)
  "Return STRING with every non-ASCII character as a \\uXXXX escape."
  (let ((text (if (multibyte-string-p string)
                  string
                (decode-coding-string string 'utf-8))))
    (if (not (string-match-p "[^[:ascii:]]" text))
        text
      (mapconcat
       (lambda (char)
         (cond
          ((< char 128) (char-to-string char))
          ((< char #x10000) (format "\\u%04x" char))
          (t (let ((v (- char #x10000)))
               (format "\\u%04x\\u%04x"
                       (+ #xD800 (ash v -10))
                       (+ #xDC00 (logand v #x3FF)))))))
       text ""))))

(defun jev--serialize (object)
  "Return OBJECT as a JSON string containing only ASCII.

`json-serialize' emits non-ASCII characters as themselves.  The
request travels as bytes, and every layer it passes -- the
transport, a log, a proxy someone plugs in through
`jev-http-function' -- is simpler to get right when the whole of
it is ASCII; JSON escapes carry the same text with no byte above
127 anywhere."
  (jev--escape-non-ascii (json-serialize object)))

(defun jev--state-json (state)
  "Return STATE ready to be serialized.
A string is sent as it is; anything else goes through `jev--json'."
  (if (stringp state) state (jev--json state)))

(defun jev--decode-json (body)
  "Decode the JSON string BODY, or signal `jev-response-error'."
  (let ((decoded (condition-case err
                     (json-parse-string body :object-type 'alist
                                        :array-type 'array
                                        :null-object nil :false-object nil)
                   (error (jev--signal 'jev-response-error
                                       (format "Could not decode response: %s"
                                               (error-message-string err))
                                       :body body)))))
    (unless (consp decoded)
      (jev--signal 'jev-response-error "Response is not a JSON object" :body body))
    decoded))


;;;; Replies

(cl-defstruct (jev-answer (:constructor jev--make-answer) (:copier nil))
  "One answer inside a `jev-reply'.
TYPE is `noul', `choice' or `score'.  VALUE is a probability, a
chosen label or a score respectively."
  key type value confidence probabilities legend)

(cl-defstruct (jev-request (:constructor jev--make-request) (:copier nil))
  "A call in flight, as returned by `jev-ask'.
CANCELLED is set by `jev-cancel'; CANCELLER, when set by the
transport, stops the attempt that is running; ON-CANCEL, set by
`jev-ask', reports an abandoned request to the end hooks."
  tag cancelled canceller on-cancel)

(defun jev-cancel (request)
  "Abandon REQUEST.

Its callbacks will not run and the attempt in flight is stopped
where the transport can stop it.  The end hooks see an error of
`cancelled' as soon as something was actually stopped; where
nothing could be, a reply may still be on its way, and it is
reported when it lands.  Cancelling twice is harmless, and so is
cancelling a request that has already answered.

Return non-nil if this call was the one that cancelled it."
  (when (and (jev-request-p request) (not (jev-request-cancelled request)))
    (setf (jev-request-cancelled request) t)
    (let ((canceller (jev-request-canceller request))
          (on-cancel (jev-request-on-cancel request)))
      (setf (jev-request-canceller request) nil)
      (setf (jev-request-on-cancel request) nil)
      (when canceller (funcall canceller))
      ;; ON-CANCEL is told whether anything was really stopped: when
      ;; nothing was, the attempt is still out there and reporting it
      ;; belongs to whoever receives the reply.
      (when on-cancel (funcall on-cancel (and canceller t))))
    t))

(defun jev-cancelled-p (request)
  "Return non-nil when REQUEST has been cancelled."
  (and (jev-request-p request) (jev-request-cancelled request)))

(cl-defstruct (jev-reply (:constructor jev--make-reply) (:copier nil))
  "A decoded answer set returned by the Jev API."
  model answers usage request-id raw)

(defun jev--as-probability (value)
  "Return VALUE as a probability.
Booleans decoded from JSON stand in for a certain 1 or 0."
  (cond ((numberp value) value)
        ((eq value t) 1)
        ((null value) 0)
        (t value)))

(defun jev--legend-list (legend)
  "Return LEGEND as the rubric levels in order, lowest first.

The native API sends it as an object keyed by level index and a
gateway may send an array; both mean the same ordered list."
  (cond
   ((null legend) nil)
   ((vectorp legend) (append legend nil))
   ((jev--alist-p legend)
    (mapcar #'cdr
            (sort (copy-sequence legend)
                  (lambda (a b)
                    (< (string-to-number (jev--key-name (car a)))
                       (string-to-number (jev--key-name (car b))))))))
   (t nil)))

(defun jev--probabilities-alist (probabilities)
  "Return PROBABILITIES as an alist keyed by strings.

The native API sends an object, keyed by option label or, for a
score, by level index; a gateway may send that indexed one as an
array instead, as it does the legend.  Both mean the same thing,
so both read the same way."
  (cond
   ((vectorp probabilities)
    (let ((index -1))
      (mapcar (lambda (value)
                (cons (number-to-string (setq index (1+ index))) value))
              probabilities)))
   ((jev--alist-p probabilities)
    (mapcar (lambda (cell) (cons (jev--key-name (car cell)) (cdr cell)))
            probabilities))))

(defun jev--parse-answer (key data)
  "Return a `jev-answer' for KEY from the decoded alist DATA."
  (unless (jev--alist-p data)
    (jev--signal 'jev-response-error
                 (format "Answer %s is %S, not an object" key data)))
  (let* ((raw-type (alist-get 'type data))
         (type (cond ((stringp raw-type) (intern raw-type))
                     ((assq 'choice data) 'choice)
                     ((assq 'score data) 'score)
                     ((assq 'noul data) 'noul))))
    (unless (memq type '(noul choice score))
      (jev--signal 'jev-response-error
                   (format "Answer %s has an unknown type %S" key raw-type)))
    (let ((value (alist-get type data '--missing)))
      (when (eq value '--missing)
        ;; A missing field is an error, never a silently valid zero.
        (jev--signal 'jev-response-error
                     (format "Answer %s of type %s has no %s field" key type type)))
      (jev--make-answer
       :key key :type type
       ;; A provider may answer a noul with a boolean rather than a
       ;; probability; callers should not have to know which.
       :value (if (eq type 'noul) (jev--as-probability value) value)
       :confidence (alist-get 'confidence data)
       :probabilities (jev--probabilities-alist (alist-get 'probabilities data))
       :legend (jev--legend-list (alist-get 'legend data))))))

(defun jev--answers-of (decoded)
  "Return the answers the DECODED response holds, or signal.

A well-formed JSON object is not yet a Jev reply.  Letting one
through without answers would hand the caller an empty reply and
move the complaint into its success callback, where an accessor
raises it about a single key and says nothing about the response
as a whole."
  (let ((cell (assq 'answers decoded)))
    (cond
     ;; `{}' decodes to nil, and so does a null: an answer set with
     ;; nothing in it leaves the caller exactly as empty-handed as a
     ;; response that never mentioned one.
     ((or (null cell) (null (cdr cell)))
      (jev--signal 'jev-response-error "Response carries no answers"))
     ((not (jev--alist-p (cdr cell)))
      (jev--signal 'jev-response-error
                   (format "Response answers are %S, not an object" (cdr cell))))
     (t (cdr cell)))))

(defun jev--parse-reply (decoded request-id &optional model)
  "Return a `jev-reply' built from the DECODED response alist.
REQUEST-ID is the correlation id of the HTTP response, if any.
MODEL is the model the request was sent for, and names the reply
where the response does not name it itself.  Answers are read in
the native shape."
  (jev--make-reply
   :model (or (alist-get 'model decoded) model)
   :answers (mapcar (lambda (cell)
                      (cons (car cell) (jev--parse-answer (car cell) (cdr cell))))
                    (jev--answers-of decoded))
   :usage (alist-get 'usage decoded)
   :request-id request-id
   :raw decoded))


;;;; Reply accessors

(defun jev-answer-for (reply key)
  "Return the `jev-answer' stored under KEY in REPLY, or signal."
  (or (alist-get (if (stringp key) (intern key) key) (jev-reply-answers reply))
      (jev--signal 'jev-response-error (format "No answer for %s in reply" key))))

(defun jev-value (reply key)
  "Return the answer value for KEY in REPLY.
A noul yields a probability, a choice its label, a score its number."
  (jev-answer-value (jev-answer-for reply key)))

(defun jev-confidence (reply key)
  "Return the confidence of the answer for KEY in REPLY."
  (jev-answer-confidence (jev-answer-for reply key)))

(defun jev-probabilities (reply key)
  "Return the probability alist of the answer for KEY in REPLY.

A fresh list, so that a caller sorting or filtering it in place
does not rearrange the answer everyone else reads."
  (copy-sequence (jev-answer-probabilities (jev-answer-for reply key))))

(defun jev--probability-of (cell)
  "Return the probability held in CELL, or -1 when it is not a number.
A provider that sends something unrankable should not take the
sort down with it; it sorts last instead."
  (let ((value (cdr cell)))
    (if (numberp value) value -1)))

(defun jev-top-choices (reply key &optional n)
  "Return the options for KEY in REPLY as (LABEL . PROBABILITY), likeliest first.

With N, return at most that many.  A caller showing a shortlist
wants this; the chosen one alone is `jev-value'."
  (let ((ranked (sort (copy-sequence (jev-probabilities reply key))
                      (lambda (a b)
                        (> (jev--probability-of a) (jev--probability-of b))))))
    (if (and n (> (length ranked) n))
        (seq-take ranked n)
      ranked)))

(defun jev-legend (reply key)
  "Return the score legend of the answer for KEY in REPLY."
  (jev-answer-legend (jev-answer-for reply key)))

(defun jev-true-p (reply key &optional threshold)
  "Return non-nil when the noul answer for KEY in REPLY holds.
THRESHOLD defaults to 0.5."
  (let ((answer (jev-answer-for reply key)))
    (unless (eq (jev-answer-type answer) 'noul)
      (jev--signal 'jev-response-error (format "Answer %s is not a noul" key)))
    (let ((value (jev-answer-value answer)))
      (unless (numberp value)
        (jev--signal 'jev-response-error
                     (format "Noul %s answered %S, which is not a probability"
                             key value)))
      (>= value (or threshold 0.5)))))

(defun jev-level (reply key)
  "Return the rubric level the score answer for KEY in REPLY lands on.

A score is not a 0-1 rating: it is a weighted position on the
rubric, so a four-level question answers somewhere in 0.0-3.0.
This rounds that position to the level it is nearest, which is
usually what a caller wants to branch on; exactly half way rounds
up, every time."
  (let ((answer (jev-answer-for reply key)))
    (unless (eq (jev-answer-type answer) 'score)
      (jev--signal 'jev-response-error (format "Answer %s is not a score" key)))
    (let ((legend (jev-answer-legend answer))
          (value (jev-answer-value answer)))
      (unless legend
        (jev--signal 'jev-response-error
                     (format "Answer %s has no legend to name its level" key)))
      (unless (numberp value)
        (jev--signal 'jev-response-error
                     (format "Score %s answered %S, which is not a position on the rubric"
                             key value)))
      ;; Not `round', which rounds a half to even: 0.5 would land on
      ;; the first level and 1.5 on the third, which reads as a bug
      ;; every time someone notices it.
      (nth (min (1- (length legend)) (max 0 (floor (+ value 0.5)))) legend))))

(defun jev--usage-field (reply names)
  "Return the first of NAMES present in the usage of REPLY.

Providers disagree on the spelling, so both are tried.  A usage
block that is not an object, or a count that is not a number, is
worth nothing to a caller counting tokens and reads as nil -- and
must read as nil rather than signalling, because this is on the
path of a reply that otherwise succeeded, where a raw error would
escape into the end hooks and take the answer with it."
  (let ((usage (jev-reply-usage reply)))
    (cl-some (lambda (name)
               (let ((value (jev--field usage name)))
                 (and (numberp value) value)))
             names)))

(defun jev-input-tokens (reply)
  "Return the number of input tokens REPLY was billed for."
  (jev--usage-field reply '(input_tokens inputTokens)))

(defun jev-output-tokens (reply)
  "Return the number of output tokens REPLY reports."
  (jev--usage-field reply '(output_tokens outputTokens)))

(defun jev-usage (reply)
  "Return the usage alist of REPLY, spelled as its provider spells it."
  (jev-reply-usage reply))

(defun jev--reported-cost (reply)
  "Return the cost the provider put in REPLY, or nil.
The Vercel gateway reports both what it charged, which is zero
while free credits last, and `marketCost', what the tokens are
worth at list price.  The second is the informative one."
  (when-let* ((gateway (jev--field (jev--field (jev-reply-raw reply)
                                               'providerMetadata)
                                   'gateway))
              (value (or (jev--field gateway 'marketCost)
                         (jev--field gateway 'cost))))
    (cond ((numberp value) value)
          ((stringp value) (string-to-number value)))))

(defun jev-cost (reply)
  "Return what REPLY cost in US dollars, at list price.

Taken from the provider when it reports one, otherwise worked out
from the input tokens and `jev-input-token-price'.  A gateway
running on free credits may well have charged nothing for it."
  (or (jev--reported-cost reply)
      (when-let* ((input (jev-input-tokens reply)))
        (/ (* input jev-input-token-price) 1000000.0))))

(provide 'jev-core)
;;; jev-core.el ends here
