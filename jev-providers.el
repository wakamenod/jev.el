;;; jev-providers.el --- Provider backends for the Jev client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wakamenod

;; Author: wakamenod <wakamenod@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Jev can be reached through more than one door, and they do not speak
;; the same protocol:
;;
;;   typesafe    POST https://api.typesafe.ai/v1/systemone
;;               {model, state, questions}, answers keyed by question.
;;               Needs a console.typesafe.ai key (early access).
;;
;;   vercel      POST https://ai-gateway.vercel.sh/v4/ai/evaluation-model
;;               Model id and protocol versions travel in headers, a
;;               noul is called a "boolean", confidence arrives under
;;               providerMetadata and a score carries no legend, so the
;;               legend is reconstructed from the request.  Experimental
;;               on Vercel's side: every header is a defcustom so a
;;               protocol bump is a setting, not a patch.
;;
;;
;; Those two are all this package speaks.  The table below is how it
;; keeps them apart -- not an interface for anything outside to
;; implement.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jev-core)

(defcustom jev-provider 'typesafe
  "Which door to reach Jev through: `typesafe' or `vercel'."
  :type '(choice (const :tag "TypeSafe (api.typesafe.ai)" typesafe)
                 (const :tag "Vercel AI Gateway" vercel))
  :group 'jev)


;;;; TypeSafe, the native API

(defconst jev-systemone-path "/v1/systemone"
  "Path of the native evaluation endpoint.")

(defun jev-typesafe--url (_model)
  "Return the native endpoint URL."
  (jev--endpoint jev-systemone-path))

(defun jev-typesafe--headers (_model)
  "Return the headers for a native request."
  `(("Authorization" . ,(concat "Bearer " (jev--api-key 'typesafe (jev--provider-env 'typesafe)
                                                        jev-base-url)))
    ("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defun jev-typesafe--body (state questions model)
  "Return the native request body for STATE, QUESTIONS and MODEL."
  (jev--serialize
   (let ((out (make-hash-table :test #'equal)))
     (puthash "model" model out)
     (puthash "state" (jev--state-json state) out)
     (puthash "questions" (jev--questions-json questions) out)
     out)))

(defun jev-typesafe--parse (decoded request-id _questions &optional model)
  "Return a `jev-reply' for the native response DECODED, tagged REQUEST-ID.
MODEL is the one the request was sent for, and names the reply
where the response does not name it itself."
  (jev--parse-reply decoded request-id model))


;;;; Vercel AI Gateway

(defcustom jev-vercel-base-url "https://ai-gateway.vercel.sh"
  "Base URL of the Vercel AI Gateway."
  :type 'string
  :group 'jev)

(defcustom jev-vercel-path "/v4/ai/evaluation-model"
  "Path of the evaluation-model endpoint on the Vercel AI Gateway."
  :type 'string
  :group 'jev)

(defcustom jev-vercel-model "typesafe-ai/jev"
  "Model id sent to the Vercel AI Gateway."
  :type 'string
  :group 'jev)

(defcustom jev-vercel-protocol-headers
  '(("ai-gateway-protocol-version" . "0.0.1")
    ("ai-gateway-auth-method" . "api-key")
    ("ai-evaluation-model-specification-version" . "4"))
  "Protocol headers required by the Vercel AI Gateway.

The evaluation-model endpoint is experimental and has already
rejected pinned versions with \"Unsupported gateway protocol
version\"; when that happens, correct these values rather than the
code."
  :type '(alist :key-type string :value-type string)
  :group 'jev)

(defcustom jev-vercel-provider-options nil
  "Extra top-level fields merged into a Vercel AI Gateway request.
This is where gateway `providerOptions' such as zero data
retention go, as an alist."
  :type '(alist :key-type string)
  :group 'jev)

(defconst jev-vercel--question-types
  '((noul . "boolean") (choice . "choice") (score . "score"))
  "How question types are spelled on the gateway.")

(defun jev-vercel--url (_model)
  "Return the gateway endpoint URL."
  (concat (string-remove-suffix "/" jev-vercel-base-url) jev-vercel-path))

(defun jev-vercel--headers (model)
  "Return the headers for a gateway request for MODEL."
  (append
   `(("Authorization" . ,(concat "Bearer " (jev--api-key 'vercel (jev--provider-env 'vercel)
                                                        jev-vercel-base-url)))
     ("Content-Type" . "application/json")
     ("Accept" . "application/json")
     ("ai-model-id" . ,model))
   jev-vercel-protocol-headers))

(defun jev-vercel--question-json (question)
  "Return QUESTION encoded the way the gateway spells it."
  (let ((out (jev--question-json question)))
    (puthash "type"
             (alist-get (jev-question-type question) jev-vercel--question-types)
             out)
    out))

(defun jev-vercel--body (state questions _model)
  "Return the gateway request body for STATE and QUESTIONS.
The model id travels in a header, not in the body."
  (jev--serialize
   (let ((out (make-hash-table :test #'equal)))
     (puthash "state" (jev--state-json state) out)
     (puthash "questions"
              (jev--questions-json questions #'jev-vercel--question-json)
              out)
     (dolist (cell jev-vercel-provider-options)
       (puthash (jev--key-name (car cell)) (jev--json (cdr cell)) out))
     out)))

(defun jev-vercel--metadata-confidence (decoded key)
  "Return the confidence for KEY held in the DECODED provider metadata."
  (when-let* ((typesafe (jev--field (jev--field decoded 'providerMetadata)
                                    'typesafe))
              (confidence (jev--field typesafe 'confidence)))
    (if (numberp confidence)
        confidence
      (jev--field confidence key))))

(defun jev-vercel--legend (questions key)
  "Return the rubric of the score question KEY in QUESTIONS.

The gateway drops it, so it is reconstructed from the request.
KEY comes back from the response as a symbol whatever the caller
wrote, so questions are matched by the name that was sent."
  (let* ((name (jev--key-name key))
         (question (cl-loop for (candidate . question) in questions
                            when (equal (jev--key-name candidate) name)
                            return question)))
    (when (and (jev-question-p question)
               (eq (jev-question-type question) 'score))
      (jev-question-criteria question))))

(defun jev-vercel--parse-answer (key data decoded questions)
  "Return a `jev-answer' for KEY from the gateway payload DATA.
DECODED is the whole response and QUESTIONS the request, both
needed for what the gateway leaves out."
  (unless (jev--alist-p data)
    (jev--signal 'jev-response-error
                 (format "Answer %s is %S, not an object" key data)))
  (let* ((raw-type (or (alist-get 'type data) (alist-get 'kind data)))
         (type (pcase raw-type
                 ((or "boolean" "noul") 'noul)
                 ("choice" 'choice)
                 ("score" 'score)))
         ;; A live gateway reply spells these `probability', `choice'
         ;; and `score'; the alternatives are kept because the
         ;; endpoint is experimental and has changed before.  A field
         ;; that is present and false decodes to nil, so only the
         ;; sentinel may stand for "not there".
         (value (cl-loop for field in (pcase type
                                        ('noul '(probability boolean noul value))
                                        ('choice '(choice value))
                                        ('score '(score value)))
                         for found = (alist-get field data '--missing)
                         unless (eq found '--missing) return found
                         finally return '--missing)))
    (unless type
      (jev--signal 'jev-response-error
                   (format "Answer %s has an unknown type %S" key raw-type)))
    (when (eq value '--missing)
      (jev--signal 'jev-response-error
                   (format "Answer %s of type %s carries no value" key type)))
    (jev--make-answer
     :key key :type type
     :value (if (eq type 'noul) (jev--as-probability value) value)
     :confidence (or (alist-get 'confidence data)
                     (jev-vercel--metadata-confidence data key)
                     (jev-vercel--metadata-confidence decoded key))
     :probabilities (jev--probabilities-alist (alist-get 'probabilities data))
     :legend (or (jev--legend-list (alist-get 'legend data))
                 (jev-vercel--legend questions key)))))

(defun jev-vercel--parse (decoded request-id questions &optional model)
  "Return a `jev-reply' for the gateway response DECODED.
REQUEST-ID tags the reply and QUESTIONS is the request it answers.

MODEL is the one the request was actually sent for.  The gateway
does not name the model in its response, so the reply would
otherwise be named after whatever `jev-vercel-model' happens to
hold by the time it lands -- which is not the model that answered
if the caller passed one to `jev-ask'."
  (jev--make-reply
   :model (or (alist-get 'model decoded) model jev-vercel-model)
   :answers (mapcar (lambda (cell)
                      (cons (car cell)
                            (jev-vercel--parse-answer (car cell) (cdr cell)
                                                      decoded questions)))
                    (jev--answers-of decoded))
   :usage (alist-get 'usage decoded)
   :request-id request-id
   :raw decoded))


;;;; Registry

(defconst jev--providers
  `((typesafe
     :url ,#'jev-typesafe--url :headers ,#'jev-typesafe--headers
     :body ,#'jev-typesafe--body :parse ,#'jev-typesafe--parse
     :model ,(lambda () jev-default-model) :env "TYPESAFE_API_KEY")
    (vercel
     :url ,#'jev-vercel--url :headers ,#'jev-vercel--headers
     :body ,#'jev-vercel--body :parse ,#'jev-vercel--parse
     :model ,(lambda () jev-vercel-model) :env "AI_GATEWAY_API_KEY"))
  "The two supported backends, as an alist of (NAME . PLIST).

Internal to the package: the entries here are how `jev-provider'
picks between TypeSafe and the Vercel gateway, not a published
interface.  Both are free to change shape with the wires they
speak for.

PLIST holds :url, :headers, :body and :parse functions, a :model
thunk returning the default model id, and the :env name of the
environment variable holding the key.  :url and :headers are
called with the model id, :body with the state, the questions and
the model id, and :parse with the decoded response, the request
id, the questions and the model id -- the last two being what a
backend needs to fill in whatever its wire leaves out.")

;; `jev-vercel-provider-options' is merged into the request body
;; whole, so a file-local setting it writes fields of its own into
;; every request this session sends.
(dolist (symbol '(jev-provider jev--providers jev-vercel-base-url jev-vercel-path
                  jev-vercel-protocol-headers jev-vercel-provider-options))
  (put symbol 'risky-local-variable t))

(defun jev--provider (&optional name)
  "Return the plist of the provider NAME, defaulting to `jev-provider'."
  (let ((name (or name jev-provider)))
    (or (alist-get name jev--providers)
        (jev--signal 'jev-configuration-error
                     (format "Unknown Jev provider `%s': use `typesafe' or `vercel'"
                             name)))))

(defun jev--provider-model (&optional name)
  "Return the default model id of the provider NAME."
  (funcall (plist-get (jev--provider name) :model)))

(defun jev--provider-env (&optional name)
  "Return the environment variable holding the key for provider NAME.
The table is the one place that spells it, so a backend asks here
rather than repeating the name."
  (plist-get (jev--provider name) :env))

(provide 'jev-providers)
;;; jev-providers.el ends here
