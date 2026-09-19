;;; triage.el --- Run one real Jev request -*- lexical-binding: t; -*-

;;; Commentary:

;; A smoke test against the live API.  From the repository root:
;;
;;   make demo PROVIDER=vercel EMACS=/path/to/emacs
;;
;; PROVIDER is optional and defaults to whatever `jev-provider' is set
;; to.  The key is looked up under the login `jev' (JEV_AUTH_USER) in
;; the macOS Keychain and in ~/.authinfo.gpg, falling back to the
;; provider's environment variable (AI_GATEWAY_API_KEY or
;; TYPESAFE_API_KEY).  Set JEV_RAW=1 to print the decoded response as
;; well.

;;; Code:

(require 'jev)

(setq jev-log t)

;; -Q means no init file, so opt into the places a real configuration
;; would keep the key: the macOS Keychain, then the usual authinfo
;; files, then the provider's environment variable.
(when (eq system-type 'darwin)
  (add-to-list 'auth-sources 'macos-keychain-internet))
(setq jev-auth-source-user (or (getenv "JEV_AUTH_USER") "jev"))

(defconst triage-state
  "Subject: charged twice this month
I was billed 42 USD on the 3rd and again on the 4th. I have opened a
ticket twice and nobody has replied. This is the third month in a row."
  "The support message to triage.")

(defun triage-report (reply)
  "Print the answers in REPLY."
  (princ (format "model: %s  request-id: %s\n"
                 (jev-reply-model reply) (jev-reply-request-id reply)))
  (dolist (cell (jev-reply-answers reply))
    (let ((answer (cdr cell)))
      (princ (format "  %-9s %-6s %-10s confidence %s\n"
                     (car cell)
                     (jev-answer-type answer)
                     (jev-answer-value answer)
                     (or (jev-answer-confidence answer) "-")))))
  (princ (format "usage: %s in, %s out\n"
                 (jev-input-tokens reply) (jev-output-tokens reply)))
  (when (getenv "JEV_RAW")
    (princ (format "raw: %S\n" (jev-reply-raw reply))))
  (princ (format "\nroute to %s, urgent: %s, severity: %s\n"
                 (jev-value reply 'team)
                 (if (jev-true-p reply 'urgent) "yes" "no")
                 (jev-level reply 'severity))))

(defun triage-run ()
  "Ask Jev to triage `triage-state' and print the answers.

This uses `jev-ask', the asynchronous entry point, because that is
how the package is meant to be called: the answer arrives in a
callback, tagged with whatever context the caller passed in.  A
batch script has no command loop to return to, so it pumps events
until the callback fires; inside Emacs you would simply return."
  (let ((outcome nil))
    (jev-ask triage-state
             (jev-questions
              (team     choice "Which team should handle this message?"
                        '(("billing" . "Payments, invoices and refunds")
                          ("support" . "Account questions and how-to")
                          ("tech"    . "Bugs, errors and outages")))
              (urgent   noul   "Does this message convey urgency?")
              (severity score  "How severe is the customer's problem?"
                        '("trivial" "annoying" "blocking" "critical")))
             :tag 'ticket-42
             :success (lambda (reply _tag) (setq outcome (cons 'ok reply)))
             :error (lambda (err _tag) (setq outcome (cons 'error err))))
    (let ((deadline (+ (float-time) 60)))
      (while (and (null outcome) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (pcase outcome
      (`(ok . ,reply) (triage-report reply))
      (`(error . ,err)
       (princ (format "%s: %s%s\n"
                      (car err) (jev-error-message err)
                      (if (jev-error-status err)
                          (format " (HTTP %s)" (jev-error-status err))
                        "")))
       (kill-emacs 1))
      (_ (princ "no answer within 60s\n") (kill-emacs 1)))))

(triage-run)

;;; triage.el ends here
