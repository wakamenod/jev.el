;;; jev-usage.el --- What this session has spent on Jev -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wakamenod

;; Author: wakamenod <wakamenod@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A Jev call is cheap enough to ignore once and worth watching when a
;; command fans out over a hundred org headings, or sends the same
;; state again for every question instead of asking them together.
;; What it actually costs is TypeSafe's to publish; this only counts
;; what went out and reports what the provider says it was worth.
;;
;; This keeps a running total per provider, fed by
;; `jev-request-end-functions', and `jev-usage-report' shows it.
;; Nothing leaves Emacs and nothing is written to disk.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jev-core)

(defcustom jev-track-usage t
  "Whether to keep a running total of what this session has spent."
  :type 'boolean
  :group 'jev)

(defvar jev-session-usage nil
  "Totals for this session, as an alist of (PROVIDER . PLIST).
PLIST holds :requests, :errors, :input-tokens, :output-tokens,
:cost and :since.  Reset it with `jev-usage-reset'.")

(defun jev--usage-entry (provider)
  "Return the totals for PROVIDER, creating them if needed."
  (or (alist-get provider jev-session-usage)
      (let ((entry (list :requests 0 :errors 0 :input-tokens 0
                         :output-tokens 0 :cost 0.0 :since (current-time))))
        (push (cons provider entry) jev-session-usage)
        entry)))

(defun jev--usage-add (entry key amount)
  "Add AMOUNT to KEY in ENTRY, in place.
Written with `plist-put' rather than `cl-incf' on the place,
which Emacs 27 cannot byte-compile."
  (plist-put entry key (+ (or (plist-get entry key) 0) amount)))

(defun jev--usage-record (info)
  "Add one finished request, described by INFO, to the totals."
  (when jev-track-usage
    (let ((entry (jev--usage-entry (or (plist-get info :provider) 'unknown))))
      (jev--usage-add entry :requests 1)
      (when (plist-get info :error)
        (jev--usage-add entry :errors 1))
      (jev--usage-add entry :input-tokens (or (plist-get info :input-tokens) 0))
      (jev--usage-add entry :output-tokens (or (plist-get info :output-tokens) 0))
      (jev--usage-add entry :cost (or (plist-get info :cost) 0.0)))))

(add-hook 'jev-request-end-functions #'jev--usage-record)

(defun jev-usage-totals ()
  "Return the totals across every provider, as a plist."
  (let ((requests 0) (errors 0) (input 0) (output 0) (cost 0.0))
    (pcase-dolist (`(,_ . ,entry) jev-session-usage)
      (cl-incf requests (plist-get entry :requests))
      (cl-incf errors (plist-get entry :errors))
      (cl-incf input (plist-get entry :input-tokens))
      (cl-incf output (plist-get entry :output-tokens))
      (cl-incf cost (plist-get entry :cost)))
    (list :requests requests :errors errors :input-tokens input
          :output-tokens output :cost cost)))

(defun jev--usage-line (label entry)
  "Return one formatted line of totals, LABEL naming ENTRY."
  (format "%-12s %5d requests  %4d failed  %9s in / %7s out  $%.6f"
          label
          (plist-get entry :requests)
          (plist-get entry :errors)
          (plist-get entry :input-tokens)
          (plist-get entry :output-tokens)
          (plist-get entry :cost)))

;;;###autoload
(defun jev-usage-report (&optional detailed)
  "Report what this session has spent on Jev.

With a prefix argument, or DETAILED non-nil, break the totals
down per provider in a buffer instead of one line.  Costs are at
list price: a gateway running on free credits may have charged
less."
  (interactive "P")
  (let ((totals (jev-usage-totals)))
    (cond
     ((zerop (plist-get totals :requests))
      (message "jev: no requests yet this session"))
     (detailed
      (with-current-buffer (get-buffer-create "*jev-usage*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Jev usage this session, at list price\n\n")
          (pcase-dolist (`(,provider . ,entry)
                         (reverse jev-session-usage))
            (insert (jev--usage-line (symbol-name provider) entry) "\n")
            (insert (format "%-12s since %s\n" ""
                            (format-time-string "%H:%M:%S"
                                                (plist-get entry :since)))))
          (insert "\n" (jev--usage-line "total" totals) "\n")
          (special-mode))
        (display-buffer (current-buffer))))
     (t
      (message "jev: %d requests, %d input tokens, about $%.6f this session%s"
               (plist-get totals :requests)
               (plist-get totals :input-tokens)
               (plist-get totals :cost)
               (if (> (plist-get totals :errors) 0)
                   (format " (%d failed)" (plist-get totals :errors))
                 ""))))))

;;;###autoload
(defun jev-usage-reset ()
  "Forget what this session has spent."
  (interactive)
  (setq jev-session-usage nil)
  (when (called-interactively-p 'interactive)
    (message "jev: usage totals reset")))

;;;###autoload
(defun jev-estimate-cost (state &optional calls)
  "Return the estimated cost of sending STATE, CALLS times.

A rough guide for a command about to fan out: it measures the
state as it would be sent and counts four characters to the
token, which is close enough to decide whether something is worth
confirming first.  The questions add a little on top, and so does
a fixed per-request overhead; neither scales with CALLS the way
the state does.

Text outside ASCII is measured as the six characters of the
escape that carries it, where four to the token is the wrong rule
anyway -- Japanese runs closer to one.  The two errors point
opposite ways and roughly cancel, but neither is a measurement:
treat a non-ASCII estimate as the order of magnitude it is."
  (let* ((wrapped (let ((object (make-hash-table :test #'equal)))
                    ;; Measured inside an object, both because that is
                    ;; how the state travels and because Emacs 27
                    ;; refuses to serialize a bare string.
                    (puthash "state" (jev--state-json state) object)
                    object))
         (tokens (/ (length (jev--serialize wrapped)) 4)))
    (/ (* tokens (or calls 1) jev-input-token-price) 1000000.0)))

(provide 'jev-usage)
;;; jev-usage.el ends here
