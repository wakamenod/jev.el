;;; jev-tests.el --- Tests for jev.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The transport is replaced with `jev-http-function' throughout, so
;; the suite never touches the network -- except for the last section,
;; which answers itself on a local socket so that the HTTP client is
;; read against real connections, in every shape a reply can arrive.

;;; Code:

(require 'ert)
(require 'jev)

(defvar jev-tests--requests nil
  "Requests captured by the stub transport, newest first.
Each is a plist of :url, :headers and the decoded :body.")

(defun jev-tests--stub (results)
  "Return a transport stub replying with RESULTS, one per attempt."
  (let ((remaining results))
    (lambda (url headers body _timeout sync callback)
      (push (list :url url :headers headers
                  :body (json-parse-string body :object-type 'alist))
            jev-tests--requests)
      (let ((result (or (pop remaining) (error "Stub ran out of results"))))
        (if sync result (progn (funcall callback result) nil))))))

(defmacro jev-tests--with-stub (results &rest body)
  "Run BODY with the transport stubbed to reply with RESULTS."
  (declare (indent 1))
  `(let ((jev-tests--requests nil)
         (jev-api-key "test-key")
         (jev-retry-initial-delay 0)
         (jev-http-function (jev-tests--stub ,results)))
     ,@body))

(defun jev-tests--wait-for (predicate)
  "Run timers until PREDICATE returns non-nil, or two seconds pass.
Asynchronous retries are scheduled with `run-at-time', so a test
that expects one has to let the timers run."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sleep-for 0.01))))

(defun jev-tests--key-function (provider)
  "Return a key that names the PROVIDER it was asked for."
  (format "key-for-%s" provider))

(defun jev-tests--last-request ()
  "Return the most recent captured request."
  (car jev-tests--requests))

(defun jev-tests--header (name)
  "Return header NAME of the most recent captured request."
  (alist-get name (plist-get (jev-tests--last-request) :headers) nil nil #'equal))

(defun jev-tests--ok (alist)
  "Return a 200 result whose body is ALIST encoded as JSON."
  (list :status 200 :headers '(("request-id" . "req_1"))
        :body (json-serialize alist)))

(defconst jev-tests--reply
  '((model . "jev-1")
    (answers
     (team (type . "choice") (choice . "billing")
           (confidence . 0.93) (probabilities (billing . 0.93) (tech . 0.07)))
     (urgent (type . "noul") (noul . 0.81))
     (severity (type . "score") (score . 0.66) (confidence . 0.7)
               (legend . ["low" "mid" "high"])))
    (usage (input_tokens . 42) (output_tokens . 3)))
  "A representative successful response.")


;;;; Questions

(ert-deftest jev-test-question-validation ()
  (should-error (jev-noul "") :type 'jev-invalid-question)
  (should-error (jev-choice "Pick" '(("only"))) :type 'jev-invalid-question)
  (should-error (jev-choice "Pick" '(("a") ("a"))) :type 'jev-invalid-question)
  (should-error (jev-score "Rate" '("only")) :type 'jev-invalid-question)
  (should-error (jev-score "Rate" (make-list 11 "level")) :type 'jev-invalid-question)
  (should (jev-question-p (jev-choice "Pick" '("a" "b"))))
  (should (jev-question-p (jev-score "Rate" ["a" "b"]))))

(ert-deftest jev-test-questions-macro ()
  (let ((questions (jev-questions
                    (urgent noul "Urgent?")
                    (team choice "Which team?" '(("billing") ("tech"))))))
    (should (equal (mapcar #'car questions) '(urgent team)))
    (should (eq (jev-question-type (alist-get 'team questions)) 'choice))))

(ert-deftest jev-test-payload-shape ()
  (let* ((payload (jev--payload
                   '((subject . "Charged twice") (amount . 42))
                   (jev-questions
                    (urgent noul "Urgent?")
                    (team choice "Which team?" '(("billing" . "Money") ("tech")))
                    (severity score "How bad?" '("low" "mid" "high")))
                   "jev-latest"))
         (decoded (json-parse-string payload :object-type 'alist))
         (questions (alist-get 'questions decoded)))
    (should (equal (alist-get 'model decoded) "jev-latest"))
    (should (equal (alist-get 'subject (alist-get 'state decoded)) "Charged twice"))
    ;; A noul without criteria sends none at all.
    (should (equal (alist-get 'type (alist-get 'urgent questions)) "noul"))
    (should-not (assq 'criteria (alist-get 'urgent questions)))
    ;; Choice criteria are an object; a missing description is null, not "".
    (let ((criteria (alist-get 'criteria (alist-get 'team questions))))
      (should (equal (alist-get 'billing criteria) "Money"))
      (should (eq (alist-get 'tech criteria) :null)))
    ;; Score criteria are an ordered array: the order is the rubric.
    (should (equal (alist-get 'criteria (alist-get 'severity questions))
                   ["low" "mid" "high"]))))

(ert-deftest jev-test-state-encoding ()
  (let ((state (alist-get 'state (json-parse-string
                                  (jev--payload '((tags . ("a" "b")) (done . t) (note . nil))
                                                (jev-questions (q noul "?"))
                                                nil)
                                  :object-type 'alist))))
    ;; A plain list becomes an array, not an object.
    (should (equal (alist-get 'tags state) ["a" "b"]))
    (should (eq (alist-get 'done state) t))
    (should (eq (alist-get 'note state) :null))))


;;;; Replies

(ert-deftest jev-test-ask-sync-parses-reply ()
  (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
    (let ((reply (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))))
      (should (equal (jev-reply-model reply) "jev-1"))
      (should (equal (jev-value reply 'team) "billing"))
      (should (equal (jev-confidence reply 'team) 0.93))
      (should (equal (alist-get "tech" (jev-probabilities reply 'team) nil nil #'equal) 0.07))
      (should (jev-true-p reply 'urgent))
      (should-not (jev-true-p reply 'urgent 0.9))
      (should (equal (jev-legend reply 'severity) '("low" "mid" "high")))
      (should (equal (jev-reply-request-id reply) "req_1"))
      (should (equal (alist-get 'input_tokens (jev-usage reply)) 42)))))

(ert-deftest jev-test-ask-async-passes-tag ()
  (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
    (let (seen)
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :tag 'issue-42
               :success (lambda (reply tag) (setq seen (cons (jev-value reply 'team) tag))))
      ;; The stub answered inside `jev-ask'; the caller still hears
      ;; about it only after `jev-ask' has returned.
      (should-not seen)
      (jev-tests--wait-for (lambda () seen))
      (should (equal seen '("billing" . issue-42))))))

(ert-deftest jev-test-missing-answer-field-is-an-error ()
  (jev-tests--with-stub (list (jev-tests--ok '((model . "jev-1")
                                               (answers (team (type . "choice"))))))
    (should-error (jev-ask-sync "state" (jev-questions (q noul "?")))
                  :type 'jev-response-error)))


;;;; Errors and retries

(ert-deftest jev-test-status-maps-to-error ()
  (dolist (case '((401 . jev-auth-error)
                  (422 . jev-validation-error)
                  (429 . jev-rate-limit-error)
                  (529 . jev-overloaded-error)
                  (400 . jev-api-error)))
    (jev-tests--with-stub (make-list (1+ jev-max-retries)
                                     (list :status (car case)
                                           :headers '(("request-id" . "req_2"))
                                           :body "{\"message\":\"nope\"}"))
      (let ((err (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                               :type (cdr case))))
        (should (equal (jev-error-message err) "nope"))
        (should (equal (jev-error-status err) (car case)))
        (should (equal (jev-error-request-id err) "req_2"))))))

(ert-deftest jev-test-retries-then-succeeds ()
  (jev-tests--with-stub (list (list :status 529 :headers nil :body "{}")
                              (jev-tests--ok jev-tests--reply))
    (should (equal (jev-value (jev-ask-sync "s" (jev-questions (q noul "?"))) 'team)
                   "billing"))
    (should (= (length jev-tests--requests) 2))))

(ert-deftest jev-test-retries-are-bounded ()
  (let ((jev-max-retries 2))
    (jev-tests--with-stub (make-list 3 (list :status 429 :headers nil :body "{}"))
      (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                    :type 'jev-rate-limit-error)
      (should (= (length jev-tests--requests) 3)))))

(ert-deftest jev-test-connection-error-reaches-errback ()
  (jev-tests--with-stub (make-list 3 (list :error 'timeout :message "too slow"))
    (let (seen)
      (jev-ask "s" (jev-questions (q noul "?"))
               :tag 'ctx
               :error (lambda (err tag) (setq seen (cons (car err) tag))))
      (jev-tests--wait-for (lambda () seen))
      (should (equal seen '(jev-timeout-error . ctx)))
      (should (= (length jev-tests--requests) 3)))))

(ert-deftest jev-test-missing-api-key-is-a-configuration-error ()
  (let ((jev-api-key nil)
        (process-environment (cons "TYPESAFE_API_KEY=" process-environment))
        (auth-sources nil))
    (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                  :type 'jev-configuration-error)))


;;;; Providers

(ert-deftest jev-test-vercel-request-shape ()
  (let ((jev-provider 'vercel)
        (process-environment (cons "AI_GATEWAY_API_KEY=vck_test" process-environment)))
    (jev-tests--with-stub (list (jev-tests--ok '((answers (urgent (type . "boolean")
                                                                 (boolean . 0.8))))))
      (let ((jev-api-key nil))
        (jev-ask-sync "s" (jev-questions
                           (urgent noul "Urgent?")
                           (severity score "How bad?" '("low" "high")))))
      (let ((body (plist-get (jev-tests--last-request) :body)))
        (should (equal (plist-get (jev-tests--last-request) :url)
                       "https://ai-gateway.vercel.sh/v4/ai/evaluation-model"))
        ;; The model id travels in a header, not in the body.
        (should-not (assq 'model body))
        (should (equal (jev-tests--header "ai-model-id") "typesafe-ai/jev"))
        (should (equal (jev-tests--header "ai-evaluation-model-specification-version") "4"))
        (should (equal (jev-tests--header "Authorization") "Bearer vck_test"))
        ;; A noul is called a boolean on the gateway; a score is not renamed.
        (let ((questions (alist-get 'questions body)))
          (should (equal (alist-get 'type (alist-get 'urgent questions)) "boolean"))
          (should (equal (alist-get 'type (alist-get 'severity questions)) "score"))
          (should (equal (alist-get 'criteria (alist-get 'severity questions))
                         ["low" "high"])))))))

(ert-deftest jev-test-vercel-response-shape ()
  (let ((jev-provider 'vercel)
        (jev-api-key "vck_test"))
    (jev-tests--with-stub
        (list (list :status 200 :headers '(("x-vercel-id" . "iad1::abc"))
                    :body (json-serialize
                           '((answers
                              (urgent (type . "boolean") (boolean . 0.8)
                                      (providerMetadata (typesafe (confidence . 0.91))))
                              (severity (type . "score") (score . 0.66)))
                             (usage (input_tokens . 12))))))
      (let* ((jev-api-key "vck_test")
             (questions (jev-questions
                         (urgent noul "Urgent?")
                         (severity score "How bad?" '("low" "mid" "high"))))
             (reply (jev-ask-sync "s" questions)))
        (should (equal (jev-value reply 'urgent) 0.8))
        ;; Confidence only exists in provider metadata on this wire.
        (should (equal (jev-confidence reply 'urgent) 0.91))
        ;; The gateway drops the legend, so it comes from the request.
        (should (equal (jev-legend reply 'severity) '("low" "mid" "high")))
        (should (equal (jev-reply-request-id reply) "iad1::abc"))
        (should (equal (jev-reply-model reply) "typesafe-ai/jev"))))))

(ert-deftest jev-test-vercel-reply-is-named-after-the-model-that-answered ()
  "A gateway reply names the model that was asked, not the default."
  (let ((jev-provider 'vercel)
        (jev-api-key "vck_test"))
    ;; The gateway does not name the model in its response at all.
    (jev-tests--with-stub (list (jev-tests--ok
                                 '((answers (urgent (type . "boolean")
                                                    (probability . 0.8))))))
      (let ((reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?"))
                                 :model "jev-1.13.0")))
        (should (equal (jev-tests--header "ai-model-id") "jev-1.13.0"))
        ;; It was sent for that model, so that is the model it answers for.
        (should (equal (jev-reply-model reply) "jev-1.13.0"))))))

(ert-deftest jev-test-reply-model-survives-a-provider-switch ()
  "The model that answered is not the one `jev-vercel-model\\=' holds later."
  (let ((jev-provider 'vercel)
        (jev-api-key "vck_test")
        reply)
    (jev-tests--with-stub (list (jev-tests--ok
                                 '((answers (urgent (type . "boolean")
                                                    (probability . 0.8))))))
      (setq reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?")))))
    ;; Whatever the setting becomes afterwards, the reply keeps its own.
    (let ((jev-vercel-model "something-else"))
      (should (equal (jev-reply-model reply) "typesafe-ai/jev")))))

(ert-deftest jev-test-typesafe-reply-model-falls-back-to-what-was-sent ()
  "A native response that omits the model is named after the request."
  (let ((jev-api-key "k"))
    (jev-tests--with-stub (list (jev-tests--ok
                                 '((answers (urgent (type . "noul") (noul . 0.8))))))
      (let ((reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?")))))
        (should (equal (jev-reply-model reply) jev-default-model))))
    ;; A response that does name one still wins.
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
      (let ((reply (jev-ask-sync "s" (jev-questions (q noul "?")) :model "asked-for")))
        (should (equal (jev-reply-model reply) "jev-1"))))))

(ert-deftest jev-test-a-score-rubric-must-be-ordered ()
  "A hash table has no order, so it cannot be a rubric."
  (should-error (jev-score "How bad?" (make-hash-table :test #'equal))
                :type 'jev-invalid-question)
  ;; A choice is unordered, so there it is fine.
  (should (jev-choice "Which?" (let ((h (make-hash-table :test #'equal)))
                                 (puthash "a" nil h) (puthash "b" nil h) h)))
  ;; Both of the ordered shapes still work.
  (should (equal (jev-question-criteria (jev-score "?" '("low" "high")))
                 '("low" "high")))
  (should (equal (jev-question-criteria (jev-score "?" ["low" "high"]))
                 '("low" "high"))))

(ert-deftest jev-test-a-reply-without-answers-is-an-error ()
  "A well-formed JSON object is not yet a Jev reply."
  (let ((jev-api-key "k"))
    (jev-tests--with-stub (list (list :status 200 :headers nil
                                      :body "{\"unrelated\":true}"))
      (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                    :type 'jev-response-error))
    ;; Not an object either.
    (jev-tests--with-stub (list (list :status 200 :headers nil
                                      :body "{\"answers\":[]}"))
      (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                    :type 'jev-response-error))
    ;; And the same on the gateway, which parses its own way.
    (let ((jev-provider 'vercel))
      (jev-tests--with-stub (list (list :status 200 :headers nil
                                        :body "{\"usage\":{}}"))
        (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                      :type 'jev-response-error)))))

(ert-deftest jev-test-reply-without-answers-reaches-errback ()
  "It is a failure like any other, so it does not reach SUCCESS."
  (let ((jev-api-key "k")
        seen)
    (jev-tests--with-stub (list (list :status 200 :headers nil
                                      :body "{\"unrelated\":true}"))
      (jev-ask "s" (jev-questions (q noul "?"))
               :success (lambda (&rest _) (setq seen 'success))
               :error (lambda (err _tag) (setq seen err)))
      (jev-tests--wait-for (lambda () seen)))
    (should (eq (car seen) 'jev-response-error))))

(ert-deftest jev-test-a-level-exactly-half-way-rounds-up ()
  "`round\\=' rounds a half to even; a rubric must not."
  (let ((reply (jev--parse-reply
                '((answers (sev (type . "score") (score . 0.5)
                                (legend . ["low" "mid" "high"]))))
                nil)))
    (should (equal (jev-level reply 'sev) "mid")))
  (let ((reply (jev--parse-reply
                '((answers (sev (type . "score") (score . 1.5)
                                (legend . ["low" "mid" "high"]))))
                nil)))
    (should (equal (jev-level reply 'sev) "high")))
  ;; And it still clamps at both ends.
  (let ((reply (jev--parse-reply
                '((answers (sev (type . "score") (score . 9.0)
                                (legend . ["low" "mid" "high"]))))
                nil)))
    (should (equal (jev-level reply 'sev) "high"))))

(ert-deftest jev-test-empty-object-survives-being-written-into ()
  "Whatever the constant holds, it still encodes as `{}\\='."
  (should (equal (jev--serialize (jev--json jev-empty-object)) "{}"))
  (unwind-protect
      (progn
        (puthash "oops" 1 jev-empty-object)
        (should (equal (jev--serialize (jev--json jev-empty-object)) "{}")))
    (clrhash jev-empty-object))
  ;; And it is still an empty object nested inside a state.
  (should (equal (jev--serialize (jev--json `(("a" . ,jev-empty-object))))
                 "{\"a\":{}}")))

(ert-deftest jev-test-answer-for-and-error-body ()
  "The two accessors nothing else in the suite reaches."
  (let ((reply (jev--parse-reply jev-tests--reply "req_1")))
    ;; A string key names the same answer as the symbol.
    (should (eq (jev-answer-for reply "team") (jev-answer-for reply 'team)))
    (should (equal (jev-answer-type (jev-answer-for reply 'team)) 'choice))
    (should-error (jev-answer-for reply 'nope) :type 'jev-response-error))
  (let ((jev-api-key "k"))
    (jev-tests--with-stub (list (list :status 422 :headers nil
                                      :body "{\"message\":\"bad question\"}"))
      (condition-case err (jev-ask-sync "s" (jev-questions (q noul "?")))
        (jev-error
         (should (equal (jev-error-body err) "{\"message\":\"bad question\"}"))
         (should (equal (jev-error-status err) 422)))))))

(ert-deftest jev-test-usage-report ()
  "`jev-usage-report\\=' reports, in one line and in a buffer."
  (let ((jev-session-usage nil))
    ;; Nothing yet.
    (should (string-match-p "no requests yet" (jev-usage-report)))
    (jev--usage-record '(:provider vercel :input-tokens 100 :output-tokens 5
                                   :cost 0.0001))
    (jev--usage-record '(:provider typesafe :input-tokens 42 :error t))
    (should (string-match-p "2 requests" (jev-usage-report)))
    (should (string-match-p "1 failed" (jev-usage-report)))
    (unwind-protect
        (progn
          ;; Twice, because the buffer is read-only once it exists.
          (jev-usage-report t)
          (jev-usage-report t)
          (with-current-buffer "*jev-usage*"
            (should (string-match-p "^vercel" (buffer-string)))
            (should (string-match-p "^typesafe" (buffer-string)))
            (should (string-match-p "^total" (buffer-string)))
            ;; Rendered afresh each time, not appended to.
            (should (equal 1 (how-many "^total" (point-min) (point-max))))))
      (when (get-buffer "*jev-usage*") (kill-buffer "*jev-usage*")))))

(ert-deftest jev-test-unknown-provider ()
  (let ((jev-provider 'nope))
    (should-error (jev--provider) :type 'jev-configuration-error)))

(ert-deftest jev-test-per-provider-api-key ()
  "`jev-api-key' can hold one key per provider, no environment needed."
  (let ((jev-api-key '((typesafe . "sk-native")
                       (vercel . jev-tests--key-function)))
        (auth-sources nil)
        (process-environment (list "TYPESAFE_API_KEY=" "AI_GATEWAY_API_KEY=")))
    (should (equal (jev--api-key 'typesafe "TYPESAFE_API_KEY") "sk-native"))
    ;; A function is called with the provider it is being asked about.
    (should (equal (jev--api-key 'vercel "AI_GATEWAY_API_KEY") "key-for-vercel"))
    ;; A provider missing from the alist still falls back, and then fails.
    (let ((jev-api-key '((vercel . "vck_gateway"))))
      (should-error (jev--api-key 'typesafe "TYPESAFE_API_KEY")
                    :type 'jev-configuration-error))))

(ert-deftest jev-test-use-provider ()
  (let ((jev-provider 'typesafe))
    (should (eq (jev-use-provider 'vercel) 'vercel))
    (should (eq jev-provider 'vercel))
    (should-error (jev-use-provider 'nope) :type 'jev-configuration-error)
    ;; A rejected provider leaves the setting alone.
    (should (eq jev-provider 'vercel))))

;;;; Key handling

(ert-deftest jev-test-key-never-reaches-the-log ()
  "Logging is verbose on purpose; it must still not print the key."
  (let ((jev-log t)
        (jev-log-buffer-name "*jev-test-log*")
        (jev-api-key "vck_secret_do_not_print"))
    (unwind-protect
        (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
          (let ((jev-api-key "vck_secret_do_not_print"))
            (jev-ask-sync "s" (jev-questions (q noul "?"))))
          (with-current-buffer "*jev-test-log*"
            (should (> (buffer-size) 0))
            (should-not (search-forward "vck_secret" nil t))))
      (when (get-buffer "*jev-test-log*") (kill-buffer "*jev-test-log*")))))

(ert-deftest jev-test-key-and-endpoints-are-risky ()
  "A file-local value must not redirect the key to another host."
  (dolist (symbol '(jev-api-key jev-base-url jev-provider jev--providers
                    jev-vercel-base-url jev-vercel-path jev-http-function
                    jev-vercel-provider-options jev-log jev-log-buffer-name))
    (should (get symbol 'risky-local-variable))))

;;;; A recorded live reply
;;
;; Captured from ai-gateway.vercel.sh on 2026-09-19.  It is the only
;; authority on how that wire really looks, so it is checked verbatim.

(defconst jev-tests--vercel-live
  '((answers
     (team (type . "choice") (choice . "billing")
           (probabilities (support . 0) (billing . 1) (tech . 0)))
     (urgent (type . "boolean") (probability . 0.87))
     (severity (type . "score") (score . 1.9)
               ;; Level indices, not labels: \0 is the symbol named "0".
               (probabilities (\0 . 0) (\1 . 0.23) (\2 . 0.64) (\3 . 0.13))))
    (rounding (probabilityDecimals . 2) (scoreDecimals . 2))
    (usage (inputTokens . 442) (outputTokens . 68))
    (warnings . [])
    (providerMetadata
     (typesafe (confidence (team . 1) (severity . 0.64)))
     (gateway (cost . "0") (marketCost . "0.000018564")
              (generationId . "gen_01M2VK04DM8K2JF7A1TS6QQ1PD"))))
  "A real answer set from the Vercel AI Gateway.")

(ert-deftest jev-test-vercel-live-reply ()
  (let ((jev-provider 'vercel)
        (jev-api-key "vck_test"))
    (jev-tests--with-stub
        (list (list :status 200
                    :headers '(("x-vercel-id" . "hnd1::cle1::bcdf4"))
                    :body (json-serialize jev-tests--vercel-live)))
      (let* ((jev-api-key "vck_test")
             (reply (jev-ask-sync
                     "state"
                     (jev-questions
                      (team choice "Which team?"
                            '(("billing") ("support") ("tech")))
                      (urgent noul "Urgent?")
                      (severity score "How severe?"
                                '("trivial" "annoying" "blocking" "critical"))))))
        ;; A noul arrives as `probability', not `boolean'.
        (should (equal (jev-value reply 'urgent) 0.87))
        (should (jev-true-p reply 'urgent))
        ;; Confidence lives in provider metadata, keyed by question, and
        ;; a noul simply has none.
        (should (equal (jev-confidence reply 'team) 1))
        (should (equal (jev-confidence reply 'severity) 0.64))
        (should-not (jev-confidence reply 'urgent))
        ;; A score is a position on the rubric, not a 0-1 rating.
        (should (equal (jev-value reply 'severity) 1.9))
        (should (equal (jev-level reply 'severity) "blocking"))
        (should (equal (jev-legend reply 'severity)
                       '("trivial" "annoying" "blocking" "critical")))
        ;; Its probabilities are keyed by level index.
        (should (equal (alist-get "2" (jev-probabilities reply 'severity)
                                  nil nil #'equal)
                       0.64))
        ;; The gateway spells usage in camelCase; the accessors hide that.
        (should (equal (jev-input-tokens reply) 442))
        (should (equal (jev-output-tokens reply) 68))))))

(ert-deftest jev-test-usage-accessors-span-providers ()
  (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
    (let ((reply (jev-ask-sync "s" (jev-questions (q noul "?")))))
      (should (equal (jev-input-tokens reply) 42))
      (should (equal (jev-output-tokens reply) 3)))))

(ert-deftest jev-test-error-array-is-read ()
  "Some providers put the reason in an errors array, with a code."
  (let ((jev-api-key "k"))
    (jev-tests--with-stub
        ;; The body a live gateway without balance really returned.
        (make-list (1+ jev-max-retries)
                   (list :status 402 :headers nil
                         :body (concat "{\"errors\":[{\"message\":\"Insufficient balance;"
                                       " add money to your gateway or use BYOK\",\"code\":2021}],"
                                       "\"success\":false,\"result\":{},\"messages\":[]}")))
      (let ((err (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                               :type 'jev-billing-error)))
        (should (equal (jev-error-message err)
                       "Insufficient balance; add money to your gateway or use BYOK (code 2021)"))
        (should (equal (jev-error-status err) 402))))))

;;;; Cost and session usage

(ert-deftest jev-test-cost-from-provider-metadata ()
  "The gateway reports what the tokens are worth; that is what is used."
  (let ((jev-provider 'vercel)
        (jev-api-key "vck_test"))
    (jev-tests--with-stub
        (list (list :status 200 :headers nil
                    :body (json-serialize
                           (append jev-tests--vercel-live nil))))
      (let* ((jev-api-key "vck_test")
             (reply (jev-ask-sync "s" (jev-questions
                                       (team choice "?" '(("billing") ("tech")))
                                       (urgent noul "?")
                                       (severity score "?" '("a" "b" "c" "d"))))))
        (should (equal (jev-cost reply) 0.000018564))))))

(ert-deftest jev-test-cost-falls-back-to-tokens ()
  "Without a reported cost, the input tokens and the price decide."
  (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
    (let ((jev-input-token-price 0.042)
          (reply (jev-ask-sync "s" (jev-questions (q noul "?")))))
      ;; 42 input tokens at $0.042 per million.
      (should (< (abs (- (jev-cost reply) (/ (* 42 0.042) 1000000.0))) 1e-12)))))

(ert-deftest jev-test-session-usage-accumulates ()
  (let ((jev-session-usage nil)
        (jev-track-usage t))
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply)
                                (jev-tests--ok jev-tests--reply))
      (jev-ask-sync "s" (jev-questions (q noul "?")))
      (jev-ask-sync "s" (jev-questions (q noul "?"))))
    (let ((totals (jev-usage-totals)))
      (should (equal (plist-get totals :requests) 2))
      (should (equal (plist-get totals :input-tokens) 84))
      (should (equal (plist-get totals :output-tokens) 6))
      (should (equal (plist-get totals :errors) 0))
      (should (> (plist-get totals :cost) 0)))
    ;; A failed request counts as a request, and as a failure.
    (jev-tests--with-stub (make-list (1+ jev-max-retries)
                                     (list :status 401 :headers nil :body "{}"))
      (ignore-errors (jev-ask-sync "s" (jev-questions (q noul "?")))))
    (should (equal (plist-get (jev-usage-totals) :requests) 3))
    (should (equal (plist-get (jev-usage-totals) :errors) 1))
    (jev-usage-reset)
    (should (equal (plist-get (jev-usage-totals) :requests) 0))))

(ert-deftest jev-test-usage-tracking-can-be-turned-off ()
  (let ((jev-session-usage nil)
        (jev-track-usage nil))
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
      (jev-ask-sync "s" (jev-questions (q noul "?"))))
    (should (equal (plist-get (jev-usage-totals) :requests) 0))))

(ert-deftest jev-test-estimate-cost-scales-with-calls ()
  (let* ((state (make-string 4000 ?x))
         (one (jev-estimate-cost state))
         (hundred (jev-estimate-cost state 100)))
    ;; About 1000 tokens at $0.042 per million, plus the few
    ;; characters of JSON the state is wrapped in.
    (should (< 0.000042 one 0.0000425))
    (should (< (abs (- hundred (* 100 one))) 1e-12))))

;;;; Non-ASCII

(ert-deftest jev-test-payload-is-pure-ascii ()
  "A request is bytes on a binary connection, so the JSON that carries
text is kept to ASCII, and the text travels as escapes."
  (let ((body (jev--payload "どこで API キーを解決している？"
                            (jev-questions
                             (urgent noul "緊急ですか？")
                             (team choice "担当は？" '(("課金") ("技術"))))
                            "jev-latest")))
    (should-not (string-match-p "[^[:ascii:]]" body))
    ;; The escapes still carry the same text.
    (let ((decoded (json-parse-string body :object-type 'alist)))
      (should (equal (alist-get 'state decoded) "どこで API キーを解決している？"))
      (should (equal (alist-get 'instructions
                                (alist-get 'urgent (alist-get 'questions decoded)))
                     "緊急ですか？"))
      (should (alist-get '課金 (alist-get 'criteria
                                          (alist-get 'team
                                                     (alist-get 'questions decoded))
                                          nil nil #'equal)
                         nil nil #'equal)))))

(ert-deftest jev-test-escape-handles-astral-characters ()
  (let ((escaped (jev--escape-non-ascii "a🙂b")))
    (should (equal escaped "a\\ud83d\\ude42b"))
    ;; Wrapped in an object: Emacs 27 rejects a bare scalar at top level.
    (should (equal (alist-get 's (json-parse-string
                                  (concat "{\"s\":\"" escaped "\"}")
                                  :object-type 'alist))
                   "a🙂b"))))

;;;; Failures reach the caller, and the hooks balance

(ert-deftest jev-test-vercel-noul-can-be-false ()
  "A noul answered false is an answer, not a missing field."
  (let ((jev-provider 'vercel) (jev-api-key "vck_test"))
    (jev-tests--with-stub
        (list (jev-tests--ok '((answers (urgent (type . "boolean")
                                                (probability . :false))))))
      (let* ((jev-api-key "vck_test")
             (reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?")))))
        (should (equal (jev-value reply 'urgent) 0))
        (should-not (jev-true-p reply 'urgent))))))

(ert-deftest jev-test-legend-survives-string-question-keys ()
  "A question written with a string key still gets its legend back."
  (let ((jev-provider 'vercel) (jev-api-key "vck_test"))
    (jev-tests--with-stub
        (list (jev-tests--ok '((answers (sev (type . "score") (score . 1.2))))))
      (let* ((jev-api-key "vck_test")
             (reply (jev-ask-sync
                     "s" (list (cons "sev" (jev-score "How bad?"
                                                      '("low" "mid" "high")))))))
        (should (equal (jev-legend reply 'sev) '("low" "mid" "high")))
        (should (equal (jev-level reply 'sev) "mid"))))))

(ert-deftest jev-test-noul-answered-with-a-boolean ()
  "The native shape may answer a noul with true rather than a number."
  (jev-tests--with-stub
      (list (jev-tests--ok '((answers (urgent (type . "noul") (noul . t))))))
    (let ((reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?")))))
      (should (equal (jev-value reply 'urgent) 1))
      (should (jev-true-p reply 'urgent)))))

(ert-deftest jev-test-unreadable-noul-is-a-jev-error ()
  (jev-tests--with-stub
      (list (jev-tests--ok '((answers (urgent (type . "noul") (noul . "oops"))))))
    (let ((reply (jev-ask-sync "s" (jev-questions (urgent noul "Urgent?")))))
      (should-error (jev-true-p reply 'urgent) :type 'jev-response-error))))

(ert-deftest jev-test-broken-reply-reaches-errback-and-the-hooks ()
  "An error that is not a `jev-error' must not escape into the transport."
  (let ((jev-session-usage nil)
        (jev-provider 'vercel)
        (jev-api-key "vck_test")
        seen)
    (jev-tests--with-stub
        ;; `answers' is a string, which is not an answer set.
        (list (list :status 200 :headers nil :body "{\"answers\":\"nonsense\"}"))
      (let ((jev-api-key "vck_test"))
        (jev-ask "s" (jev-questions (q noul "?"))
                 :tag 'ctx
                 :error (lambda (err tag) (setq seen (cons (car err) tag))))))
    (jev-tests--wait-for (lambda () seen))
    (should (equal seen '(jev-response-error . ctx)))
    ;; The end hooks ran, so the failure was counted.
    (should (equal (plist-get (jev-usage-totals) :errors) 1))))

(ert-deftest jev-test-configuration-error-reaches-errback-asynchronously ()
  "A missing key is reported like any other failure, after returning."
  (let ((jev-api-key nil)
        (process-environment (list "TYPESAFE_API_KEY="))
        (auth-sources nil)
        seen)
    (let ((request (jev-ask "s" (jev-questions (q noul "?"))
                            :tag 'ctx
                            :error (lambda (err tag) (setq seen (cons (car err) tag))))))
      (should (jev-request-p request))
      ;; Not yet: jev-ask has to return first.
      (should-not seen)
      (jev-tests--wait-for (lambda () seen))
      (should (equal seen '(jev-configuration-error . ctx))))))

(ert-deftest jev-test-invalid-question-reaches-errback ()
  (let ((jev-api-key "k") seen)
    (jev-ask "s" (list (cons 'q (jev-noul "ok?")) (cons 'bad "not a question"))
             :error (lambda (err _tag) (setq seen (car err))))
    (jev-tests--wait-for (lambda () seen))
    (should (equal seen 'jev-invalid-question))))


(ert-deftest jev-test-transport-refusal-reaches-errback ()
  "A transport may refuse a request outright; that must not reach the caller."
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (starts 0)
        seen returned)
    (let ((jev-request-start-functions (list (lambda (_info) (cl-incf starts))))
          (jev-http-function
           (lambda (&rest _) (error "The transport refused to send this"))))
      (setq returned (jev-ask "s" (jev-questions (q noul "?"))
                              :tag 'ctx
                              :error (lambda (err tag) (setq seen (cons err tag)))))
      ;; It returned a request rather than throwing, so a caller can
      ;; still hold on to it.
      (should (jev-request-p returned))
      (should-not seen)
      (jev-tests--wait-for (lambda () seen))
      (should (eq (car (car seen)) 'jev-connection-error))
      (should (eq (cdr seen) 'ctx))
      ;; The start hook ran, so the end hook has to balance it.
      (should (equal starts 1))
      (should (equal (plist-get (jev-usage-totals) :requests) 1))
      (should (equal (plist-get (jev-usage-totals) :errors) 1)))))

(ert-deftest jev-test-a-refused-retry-still-reaches-errback-and-the-hooks ()
  "A transport that refuses a retry must not drop the request.
The retry runs from a timer, which swallows a signal: nothing
would reach ERRBACK and the end hooks would never balance the
start hook, leaving the caller waiting for an answer forever."
  (let ((jev-api-key "k")
        (jev-retry-initial-delay 0)
        (jev-max-retries 2)
        (attempts 0) (ends 0)
        seen)
    (let ((jev-request-end-functions (list (lambda (info) (cl-incf ends))))
          (jev-http-function
           (lambda (_url _headers _body _timeout _sync callback)
             (cl-incf attempts)
             (if (= attempts 1)
                 (progn (funcall callback (list :status 500 :headers nil :body "{}"))
                        nil)
               (error "The transport refused to send this")))))
      (jev-ask "s" (jev-questions (q noul "?"))
               :tag 'ctx
               :error (lambda (err tag) (setq seen (cons err tag))))
      (jev-tests--wait-for (lambda () seen))
      (should (eq (car (car seen)) 'jev-connection-error))
      (should (eq (cdr seen) 'ctx))
      ;; Refused as a connection failure, so the remaining retry was
      ;; spent on it rather than the request being abandoned.
      (should (equal attempts 3))
      (should (equal ends 1)))))

(ert-deftest jev-test-an-error-in-success-is-not-a-send-failure ()
  "A callback that throws is the caller's own error, not a failed request.

The error is reported and the request is still counted as the
answered one it was."
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (inhibit-message t)
        ;; ERT turns this on, which is a request for the debugger
        ;; rather than a report; the test wants the report.
        (debug-on-error nil)
        (ran nil)
        seen)
    ;; A stub answers inside the call that sends.  That must not put
    ;; SUCCESS inside `jev-ask', where an error from it would be
    ;; caught as a request that could not be sent.
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
      (should (jev-request-p
               (jev-ask "s" (jev-questions (q noul "?"))
                        :success (lambda (_reply _tag)
                                   (setq ran t)
                                   (error "Callback blew up"))
                        :error (lambda (err _tag) (setq seen (car err))))))
      (jev-tests--wait-for (lambda () ran))
      (should ran)
      ;; Reported as a reply, not as a failure, and not to ERRBACK.
      (should-not seen)
      (should (equal (plist-get (jev-usage-totals) :errors) 0))
      (should (equal (plist-get (jev-usage-totals) :requests) 1)))))

(ert-deftest jev-test-unencodable-state-reaches-errback ()
  "A state `json-serialize\\=' refuses is its own error, not a connection one.

A caller that retries every `jev-connection-error\\=' would be sent
round that loop forever by a state that can never be encoded."
  (let ((jev-api-key "k")
        (starts 0) (ends 0)
        seen)
    (let ((jev-request-start-functions (list (lambda (_) (cl-incf starts))))
          (jev-request-end-functions (list (lambda (_) (cl-incf ends)))))
      (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
        ;; Nothing is signalled here, however badly the state encodes.
        (jev-ask (vector 1 0.0e+NaN) (jev-questions (q noul "?"))
                 :success (lambda (&rest _) (setq seen 'success))
                 :error (lambda (err _tag) (setq seen err)))
        (jev-tests--wait-for (lambda () seen))))
    (should (eq (car seen) 'jev-invalid-state))
    ;; How `json-serialize' words its refusal is the platform's
    ;; business -- Emacs 30 names the NaN, and a build against
    ;; libjansson reports the same refusal as a failure to allocate.
    ;; What belongs to this package is that the raw error was wrapped
    ;; rather than left to escape into the caller.
    (should (string-prefix-p "Could not build the request: "
                             (jev-error-message seen)))
    ;; It never left, so neither hook has anything to say about it.
    (should (equal starts 0))
    (should (equal ends 0))))

(ert-deftest jev-test-sync-unencodable-state-is-a-jev-error ()
  "`jev-ask-sync\\=' signals a `jev-error\\=' for a state it cannot encode."
  (let ((jev-api-key "k"))
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
      (should-error (jev-ask-sync (vector 1 0.0e+NaN) (jev-questions (q noul "?")))
                    :type 'jev-invalid-state))))

(ert-deftest jev-test-a-start-hook-that-signals-still-reaches-the-end-hooks ()
  "A request a start hook took down is still one the end hooks counted."
  (let ((jev-api-key "k")
        (ends nil)
        seen)
    (let ((jev-request-start-functions (list (lambda (_) (error "Hook blew up"))))
          (jev-request-end-functions (list (lambda (info) (push info ends)))))
      (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
        (jev-ask "s" (jev-questions (q noul "?"))
                 :success (lambda (&rest _) (setq seen 'success))
                 :error (lambda (err _tag) (setq seen err)))
        (jev-tests--wait-for (lambda () seen))))
    (should (eq (car seen) 'jev-connection-error))
    ;; Exactly once, which is what makes the end hooks safe to count with.
    (should (equal (length ends) 1))
    (should (eq (car (plist-get (car ends) :error)) 'jev-connection-error))
    (should (eq (plist-get (car ends) :provider) 'typesafe))))

(ert-deftest jev-test-sync-start-hook-that-signals-reaches-the-end-hooks ()
  "`jev-ask-sync\\=' balances its hooks when a start hook signals."
  (let ((jev-api-key "k")
        (ends nil))
    (let ((jev-request-start-functions (list (lambda (_) (error "Hook blew up"))))
          (jev-request-end-functions (list (lambda (info) (push info ends)))))
      (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
        (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                      :type 'jev-connection-error)))
    (should (equal (length ends) 1))))

(ert-deftest jev-test-sync-transport-refusal-is-a-jev-error ()
  "`jev-ask-sync' signals a `jev-error', never a raw one."
  (let ((jev-api-key "k")
        (jev-http-function (lambda (&rest _) (error "The transport refused to send this"))))
    (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                  :type 'jev-connection-error)))

(ert-deftest jev-test-usage-follows-the-provider-it-was-sent-to ()
  "Switching provider mid-flight must not misfile the request."
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (jev-provider 'typesafe)
        (answer nil))
    (let ((jev-http-function
           (lambda (_u _h _b _t sync c) (if sync nil (progn (setq answer c) nil)))))
      (jev-ask "s" (jev-questions (q noul "?")))
      (setq jev-provider 'vercel)
      (funcall answer (jev-tests--ok '((answers (q (type . "noul") (noul . 0.5))))))
      (should (equal (mapcar #'car jev-session-usage) '(typesafe))))))


;;;; Cancelling

(ert-deftest jev-test-cancel-reaches-the-end-hooks ()
  "A request stopped in flight is still reported: it was sent."
  (let ((jev-api-key "k")
        (ends nil))
    ;; Binding the hook replaces the usage recorder on it; what the
    ;; totals make of a cancellation is
    ;; `jev-test-cancel-after-the-answer-counts-but-stays-quiet''s.
    (let ((jev-request-end-functions (list (lambda (info) (push info ends))))
          (jev-http-function
           (lambda (_u _h _b _t sync _c)
             (if sync nil (lambda () nil)))))
      (jev-cancel (jev-ask "s" (jev-questions (q noul "?"))))
      (should (equal (length ends) 1))
      (should (eq (car (plist-get (car ends) :error)) 'jev-cancelled))
      (should (eq (plist-get (car ends) :provider) 'typesafe)))))

(ert-deftest jev-test-cancel-reports-once ()
  "A reply landing after a cancellation is not counted a second time."
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (answer nil))
    (let ((jev-http-function
           (lambda (_u _h _b _t sync c)
             (if sync nil (progn (setq answer c) (lambda () nil))))))
      (let ((request (jev-ask "s" (jev-questions (q noul "?")))))
        (jev-cancel request)
        (funcall answer (jev-tests--ok '((answers (q (type . "noul") (noul . 0.5))))))
        (should (equal (plist-get (jev-usage-totals) :requests) 1))))))

(ert-deftest jev-test-cancel-silences-the-callbacks ()
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (pending nil)
        seen)
    ;; A transport that never answers until we let it.
    (let* ((jev-http-function
            (lambda (_url _headers _body _timeout sync callback)
              (if sync (list :status 200 :headers nil :body "{}")
                (setq pending callback)
                (lambda () (setq pending 'cancelled)))))
           (request (jev-ask "s" (jev-questions (q noul "?"))
                             :success (lambda (_r _t) (setq seen 'success))
                             :error (lambda (_e _t) (setq seen 'error)))))
      (should (jev-cancel request))
      ;; Cancelling reached the transport, and twice is harmless.
      (should (eq pending 'cancelled))
      (should-not (jev-cancel request))
      (should (jev-cancelled-p request))
      (should-not seen))))

(ert-deftest jev-test-cancel-after-the-answer-counts-but-stays-quiet ()
  (let ((jev-api-key "k")
        (jev-session-usage nil)
        (answer nil)
        seen)
    (let* ((jev-http-function
            (lambda (_url _headers _body _timeout sync callback)
              (if sync (list :status 200 :headers nil :body "{}")
                (setq answer callback)
                nil)))
           (request (jev-ask "s" (jev-questions (q noul "?"))
                             :success (lambda (_r _t) (setq seen 'success)))))
      (jev-cancel request)
      ;; The reply lands after the caller stopped caring.
      (funcall answer (list :status 200 :headers nil
                            :body (json-serialize jev-tests--reply)))
      (should-not seen)
      ;; It was still sent, so it is still counted -- as a failure.
      (should (equal (plist-get (jev-usage-totals) :requests) 1))
      (should (equal (plist-get (jev-usage-totals) :errors) 1)))))

(ert-deftest jev-test-cancel-stops-a-pending-retry ()
  (let ((jev-api-key "k")
        (jev-retry-initial-delay 0.2)
        (attempts 0)
        seen)
    (let* ((jev-http-function
            (lambda (_url _headers _body _timeout sync callback)
              (setq attempts (1+ attempts))
              (let ((result (list :status 429 :headers nil :body "{}")))
                (if sync result (progn (funcall callback result) nil)))))
           (request (jev-ask "s" (jev-questions (q noul "?"))
                             :error (lambda (_e _t) (setq seen 'error)))))
      (should (equal attempts 1))
      (jev-cancel request)
      ;; Waiting out the backoff must not wake the retry up.
      (jev-tests--wait-for (lambda () (> attempts 1)))
      (should (equal attempts 1))
      (should-not seen))))


;;;; The HTTP client, without the network

(ert-deftest jev-test-parse-headers ()
  (let ((headers (jev-http--parse-headers
                  (concat "HTTP/1.1 429 Too Many Requests\r\n"
                          "Content-Type: application/json\r\n"
                          "Retry-After: 3\r\n"
                          "X-Vercel-Id: iad1::abc\r\n"))))
    (should (equal (alist-get "retry-after" headers nil nil #'equal) "3"))
    (should (equal (alist-get "x-vercel-id" headers nil nil #'equal) "iad1::abc"))
    ;; The status line is not a header.
    (should-not (alist-get "http/1.1" headers nil nil #'equal))))

(defun jev-tests--parse (text closed)
  "Return what the client makes of the raw response TEXT so far.
CLOSED says the server has hung up after it."
  (let ((buffer (generate-new-buffer " *jev-test-http*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-buffer-multibyte nil)
          (insert (encode-coding-string text 'utf-8))
          (jev-http--parse buffer closed))
      (kill-buffer buffer))))

(ert-deftest jev-test-a-response-is-read-by-its-framing ()
  "Content-Length, chunked, and close-delimited all name the same body."
  (let ((body "{\"answers\":{}}"))
    ;; Content-Length: complete as soon as the bytes are there.
    (let ((result (jev-tests--parse
                   (format "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
                           (length body) body)
                   nil)))
      (should (equal (plist-get result :status) 200))
      (should (equal (plist-get result :body) body)))
    ;; Chunked, in two chunks with an extension and a trailer.
    (let ((result (jev-tests--parse
                   (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                           "5;ext=1\r\n{\"ans\r\n9\r\nwers\":{}}\r\n0\r\nX-Trailer: 1\r\n\r\n")
                   nil)))
      (should (equal (plist-get result :body) body)))
    ;; Neither: the body ends where the connection does, and not before.
    (should-not (jev-tests--parse (concat "HTTP/1.1 200 OK\r\n\r\n" body) nil))
    (should (equal (plist-get (jev-tests--parse (concat "HTTP/1.1 200 OK\r\n\r\n" body) t)
                              :body)
                   body))
    ;; An interim 1xx is skipped, and the reply after it is the reply.
    (should (equal (plist-get (jev-tests--parse
                               (format (concat "HTTP/1.1 100 Continue\r\n\r\n"
                                               "HTTP/1.1 201 Created\r\nContent-Length: %d\r\n\r\n%s")
                                       (length body) body)
                               nil)
                              :status)
                   201))
    ;; A body in UTF-8 comes out as the text it was.
    (let ((text "{\"a\":\"日本語\"}"))
      (should (equal (plist-get (jev-tests--parse
                                 (format "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
                                         (string-bytes text) text)
                                 nil)
                                :body)
                     text)))))

(ert-deftest jev-test-an-incomplete-response-waits-then-fails ()
  "Nothing is reported until the reply is whole; a hang-up before that is a failure.
And a failure of the connection, which is worth another attempt,
never a status the API did not send."
  (dolist (partial (list ""
                         "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n"
                         "HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\n{\"ans"
                         "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\n{\"an"))
    (should-not (jev-tests--parse partial nil))
    (let ((result (jev-tests--parse partial t)))
      (should (eq (plist-get result :error) 'connection))
      (should-not (plist-get result :status))
      (should (jev-http--retryable-p result))))
  ;; Something that is not HTTP at all is a failure however it ends.
  (dolist (closed '(nil t))
    (should (eq (plist-get (jev-tests--parse "rubbish\r\n\r\n" closed) :error) 'connection))
    (should (eq (plist-get (jev-tests--parse
                            "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n" closed)
                           :error)
                'connection))))

(ert-deftest jev-test-the-request-on-the-wire ()
  "What leaves is an HTTP/1.1 POST with the body counted in bytes."
  (let* ((target (jev-http--target "https://api.example.com/v1/systemone?x=1"))
         (text (jev-http--request-text target '(("Authorization" . "Bearer k")) "{\"s\":\"日\"}")))
    (should (equal (plist-get target :port) 443))
    (should (plist-get target :tls))
    (should-not (multibyte-string-p text))
    (should (string-prefix-p "POST /v1/systemone?x=1 HTTP/1.1\r\nHost: api.example.com\r\n" text))
    (should (string-match-p "\r\nConnection: close\r\n" text))
    (should (string-match-p "\r\nAuthorization: Bearer k\r\n" text))
    ;; Eight ASCII characters and the three bytes of the one that is not.
    (should (string-match-p "\r\nContent-Length: 11\r\n" text))
    (should (string-suffix-p (concat "\r\n\r\n" (encode-coding-string "{\"s\":\"日\"}" 'utf-8))
                             text)))
  ;; A port that is not the scheme's travels in the Host header.
  (let ((target (jev-http--target "http://127.0.0.1:8080")))
    (should (equal (plist-get target :port) 8080))
    (should (equal (plist-get target :path) "/"))
    (should (string-match-p "\r\nHost: 127.0.0.1:8080\r\n"
                            (jev-http--request-text target nil "{}"))))
  (should-error (jev-http--target "ftp://example.com/") :type 'error)
  (should-error (jev-http--target "not a url") :type 'error))

(ert-deftest jev-test-a-request-that-cannot-start-is-a-connection-failure ()
  "A URL that is not one settles as a failure, and never signals.
Synchronously and asynchronously alike, and with nothing left behind."
  (let ((buffers (length (buffer-list)))
        (result (jev-http--socket "nowhere" nil "{}" 1 t nil)))
    (should (eq (plist-get result :error) 'connection))
    (should (equal (length (buffer-list)) buffers))
    (setq result nil)
    (jev-http--socket "nowhere" nil "{}" 1 nil (lambda (answer) (setq result answer)))
    (jev-tests--wait-for (lambda () result))
    (should (eq (plist-get result :error) 'connection))
    (should (equal (length (buffer-list)) buffers))))

(ert-deftest jev-test-retry-after-and-backoff ()
  (let ((jev-retry-initial-delay 0.5))
    ;; Seconds, as every provider we have seen sends it.
    (should (equal (jev-http--retry-after '(:headers (("retry-after" . "3")))) 3))
    ;; An HTTP-date, or anything else unparseable, falls back to the backoff.
    (should-not (jev-http--retry-after
                 '(:headers (("retry-after" . "Wed, 21 Oct 2026 07:28:00 GMT")))))
    (should-not (jev-http--retry-after '(:headers (("retry-after" . "0")))))
    (should-not (jev-http--retry-after '(:headers nil)))
    ;; Backoff doubles, and Retry-After wins when it is there.
    (should (equal (jev-http--delay 0 '(:headers nil)) 0.5))
    (should (equal (jev-http--delay 1 '(:headers nil)) 1.0))
    (should (equal (jev-http--delay 0 '(:headers (("retry-after" . "7")))) 7))
    ;; However long it asks for, one minute is the most we wait.
    (should (equal (jev-http--delay 0 '(:headers (("retry-after" . "3600")))) 60))))

(ert-deftest jev-test-keyword-values-drop-their-colon ()
  (let ((state (alist-get 'state (json-parse-string
                                  (jev--payload '((mode . :emacs-lisp))
                                                (jev-questions (q noul "?"))
                                                nil)
                                  :object-type 'alist))))
    (should (equal (alist-get 'mode state) "emacs-lisp"))))

(ert-deftest jev-test-estimate-counts-what-is-sent ()
  "The estimate measures the JSON, not the Lisp printed representation."
  (let* ((state '((note . "hello")))
         (payload (jev--serialize
                   (let ((object (make-hash-table :test #'equal)))
                     (puthash "state" (jev--state-json state) object)
                     object)))
         (actual (/ (* (/ (length payload) 4) jev-input-token-price) 1000000.0)))
    (should (equal (jev-estimate-cost state) actual))
    ;; A string state is measured too; Emacs 27 cannot serialize one alone.
    (should (> (jev-estimate-cost "hello") 0))))

(ert-deftest jev-test-score-that-is-not-a-number-is-a-jev-error ()
  "A score answered with something unrankable is a reply error, not a crash."
  (let ((reply (jev--parse-reply
                '((answers (sev (type . "score") (score . "high")
                                (legend . ["a" "b" "c"]))))
                nil)))
    (should-error (jev-level reply 'sev) :type 'jev-response-error)))

(ert-deftest jev-test-top-choices ()
  (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
    (let ((reply (jev-ask-sync "s" (jev-questions (q noul "?")))))
      (should (equal (jev-top-choices reply 'team)
                     '(("billing" . 0.93) ("tech" . 0.07))))
      (should (equal (jev-top-choices reply 'team 1) '(("billing" . 0.93))))
      ;; Asking for more than there are is not an error.
      (should (equal (length (jev-top-choices reply 'team 10)) 2)))))

;;;; A recorded live reply from the native API

(defconst jev-tests--typesafe-live
  '((model . "jev-1.13.0")
    (answers
     (team (type . "choice") (choice . "billing") (confidence . 1.0)
           (probabilities (billing . 1.0) (tech . 0.0) (support . 0.0)))
     (urgent (type . "noul") (noul . 0.87))
     (severity (type . "score") (score . 1.87) (confidence . 0.65)
               ;; An object keyed by level index, not an array.
               (legend (\0 . "trivial") (\1 . "annoying")
                       (\2 . "blocking") (\3 . "critical"))
               (probabilities (\0 . 0.0) (\1 . 0.24) (\2 . 0.65) (\3 . 0.11))))
    (usage (input_tokens . 442) (output_tokens . 68)))
  "A real answer set from api.typesafe.ai, 2026-09-19.")

(ert-deftest jev-test-typesafe-live-reply ()
  (jev-tests--with-stub
      (list (list :status 200
                  :headers '(("x-typesafe-request-id" . "req_01a0b7cfcd6e"))
                  :body (json-serialize jev-tests--typesafe-live)))
    (let ((reply (jev-ask-sync
                  "state"
                  (jev-questions
                   (team choice "Which team?" '(("billing") ("tech") ("support")))
                   (urgent noul "Urgent?")
                   (severity score "How severe?"
                             '("trivial" "annoying" "blocking" "critical"))))))
      (should (equal (jev-reply-model reply) "jev-1.13.0"))
      ;; A noul arrives under its own name here, and confidence is at
      ;; the answer rather than in provider metadata.
      (should (equal (jev-value reply 'urgent) 0.87))
      (should (equal (jev-confidence reply 'team) 1.0))
      (should (equal (jev-confidence reply 'severity) 0.65))
      ;; The legend is an object keyed by index; it still reads as the
      ;; rubric, in order.
      (should (equal (jev-legend reply 'severity)
                     '("trivial" "annoying" "blocking" "critical")))
      (should (equal (jev-level reply 'severity) "blocking"))
      (should (equal (alist-get "2" (jev-probabilities reply 'severity)
                                nil nil #'equal)
                     0.65))
      (should (equal (jev-input-tokens reply) 442))
      (should (equal (jev-reply-request-id reply) "req_01a0b7cfcd6e")))))

(ert-deftest jev-test-legend-shapes ()
  (should (equal (jev--legend-list ["low" "high"]) '("low" "high")))
  (should (equal (jev--legend-list '((\1 . "high") (\0 . "low")))
                 '("low" "high")))
  (should-not (jev--legend-list nil)))

;;;; Edge cases in replies and requests

(defun jev-tests--async-stub (result)
  "Return a transport answering RESULT from a timer, as the network would."
  (lambda (url headers body _timeout sync callback)
    (push (list :url url :headers headers
                :body (json-parse-string body :object-type 'alist))
          jev-tests--requests)
    (if sync result (progn (run-at-time 0 nil callback result) nil))))

(defconst jev-tests--unreadable-usage
  '((answers (q (type . "noul") (noul . 0.5)))
    (usage (input_tokens . "442")))
  "A perfectly good answer with a token count that is not a number.")

(ert-deftest jev-test-a-usage-it-cannot-read-is-not-a-failed-request ()
  "Counting tokens happens on the path of a reply that succeeded.

A provider that spells a count as a string, or sends an array
where the usage object belongs, must cost the caller its usage --
not its answer, not the end hooks, and not, asynchronously, the
whole callback to a timer nobody is listening to."
  (let ((jev-api-key "k"))
    ;; Synchronously: the reply arrives, minus what could not be read.
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--unreadable-usage))
      (let ((reply (jev-ask-sync "s" (jev-questions (q noul "?")))))
        (should (equal (jev-value reply 'q) 0.5))
        (should-not (jev-input-tokens reply))
        (should-not (jev-cost reply))
        ;; The raw block is still handed over, spelled as it came.
        (should (equal (alist-get 'input_tokens (jev-usage reply)) "442"))))
    ;; A usage that is not an object at all reads the same way.
    (jev-tests--with-stub
        (list (list :status 200 :headers nil
                    :body (concat "{\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.5}},"
                                  "\"usage\":[1,2]}")))
      (should-not (jev-input-tokens (jev-ask-sync "s" (jev-questions (q noul "?"))))))
    ;; Asynchronously, from a timer: SUCCESS runs and the end hooks
    ;; balance, which is what the session totals are counted from.
    (let ((jev-session-usage nil)
          (ends 0)
          seen)
      (let ((jev-request-end-functions (list (lambda (_) (setq ends (1+ ends)))
                                             #'jev--usage-record))
            (jev-api-key "k")
            (jev-http-function
             (jev-tests--async-stub (jev-tests--ok jev-tests--unreadable-usage))))
        (jev-ask "s" (jev-questions (q noul "?"))
                 :success (lambda (reply _tag) (setq seen (jev-value reply 'q)))
                 :error (lambda (err _tag) (setq seen (cons 'error (car err)))))
        (jev-tests--wait-for (lambda () seen)))
      (should (equal seen 0.5))
      (should (equal ends 1))
      (should (equal (plist-get (jev-usage-totals) :requests) 1))
      (should (equal (plist-get (jev-usage-totals) :errors) 0)))))

(ert-deftest jev-test-an-end-hook-that-signals-keeps-it-to-itself ()
  "One observer must not silence the others or lose the answer.

The start hooks are deliberately allowed to take a request down
before it leaves.  By the time the end hooks run there is a reply
to deliver and a caller waiting for it, and `jev--usage-record\='
is on that same hook: a third party signalling there would stop
the totals dead and, asynchronously, escape into a timer."
  (let* ((jev-api-key "k")
         (inhibit-message t)
         (reached 0)
         (jev-request-end-functions
          (list (lambda (_) (error "First hook blew up"))
                (lambda (_) (setq reached (1+ reached))))))
    ;; Synchronously: the caller still gets the reply.
    (jev-tests--with-stub (list (jev-tests--ok jev-tests--reply))
      (should (equal (jev-value (jev-ask-sync "s" (jev-questions (q noul "?"))) 'team)
                     "billing")))
    ;; And the hook after the broken one ran anyway.
    (should (equal reached 1))
    ;; Asynchronously, from a timer: SUCCESS still runs.
    (let ((seen nil)
          (jev-http-function
           (jev-tests--async-stub (jev-tests--ok jev-tests--reply))))
      (jev-ask "s" (jev-questions (q noul "?"))
               :success (lambda (_reply _tag) (setq seen 'success))
               :error (lambda (err _tag) (setq seen (car err))))
      (jev-tests--wait-for (lambda () seen))
      (should (eq seen 'success))
      (should (equal reached 2)))))

(ert-deftest jev-test-probabilities-may-arrive-as-an-array ()
  "A score\='s probabilities are keyed by level index, and an array says
the same thing -- as it already does for the legend."
  (jev-tests--with-stub
      (list (list :status 200 :headers nil
                  :body (concat "{\"answers\":{\"sev\":{\"type\":\"score\",\"score\":1.2,"
                                "\"legend\":[\"low\",\"mid\",\"high\"],"
                                "\"probabilities\":[0.1,0.7,0.2]}}}")))
    (let ((reply (jev-ask-sync "s" (jev-questions (sev score "?" '("a" "b" "c"))))))
      (should (equal (jev-probabilities reply 'sev)
                     '(("0" . 0.1) ("1" . 0.7) ("2" . 0.2))))
      (should (equal (jev-top-choices reply 'sev 1) '(("1" . 0.7)))))))

(ert-deftest jev-test-an-empty-answer-set-is-no-answer-set ()
  "`{}\=' decodes to nil, and leaves the caller as empty-handed as a
response that never mentioned answers at all."
  (jev-tests--with-stub (list (list :status 200 :headers nil
                                    :body "{\"answers\":{}}"))
    (let ((err (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                             :type 'jev-response-error)))
      (should (equal (jev-error-message err) "Response carries no answers")))))

(ert-deftest jev-test-two-questions-cannot-share-a-key ()
  "They would travel as one, and one caller would read the other\='s answer."
  (let ((questions (list (cons 'q (jev-noul "First?"))
                         (cons "q" (jev-noul "Second?")))))
    (should-error (jev--payload "s" questions "m") :type 'jev-invalid-question)
    ;; The gateway builds its own questions object, and checks the same.
    (let ((jev-provider 'vercel))
      (should-error (jev--payload "s" questions "m") :type 'jev-invalid-question))))

(ert-deftest jev-test-the-gateway-validates-the-questions-it-is-given ()
  "Not an alist of questions is an invalid question, on either wire."
  (let ((jev-provider 'vercel))
    (should-error (jev--payload "s" nil "m") :type 'jev-invalid-question)
    (should-error (jev--payload "s" (list (cons 'q "not a question")) "m")
                  :type 'jev-invalid-question)))

(ert-deftest jev-test-a-keyword-option-loses-its-colon ()
  "`jev--key-name\=' drops it from an object key; an option reads the same."
  (should (equal (mapcar #'car (jev-question-criteria (jev-choice "?" '(:a :b))))
                 '("a" "b")))
  (should (equal (jev-question-criteria (jev-score "?" '(:low :high)))
                 '("low" "high"))))

(ert-deftest jev-test-an-answer-that-is-not-an-object ()
  "It is a malformed response, said plainly, not a wrong-type-argument."
  (let ((jev-api-key "k"))
    (dolist (provider '(typesafe vercel))
      (let ((jev-provider provider))
        (jev-tests--with-stub (list (list :status 200 :headers nil
                                          :body "{\"answers\":{\"q\":5}}"))
          (let ((err (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                                   :type 'jev-response-error)))
            (should (string-match-p "not an object" (jev-error-message err)))))))))

(ert-deftest jev-test-max-retries-that-is-not-a-number-sends-once ()
  "`jev-max-retries\=' cannot be counted down against unless it is a
whole number, and there is nothing to do with it then but send once."
  (dolist (setting '(nil -1 0))
    (let ((jev-max-retries setting))
      (jev-tests--with-stub (list (list :status 500 :headers nil :body "{}"))
        (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                      :type 'jev-api-error)
        (should (equal (length jev-tests--requests) 1))))))

(ert-deftest jev-test-an-error-under-detail-is-read ()
  "The native API says why under `detail', as an object or a string."
  (should (equal (jev--error-detail
                  (concat "{\"detail\":{\"error_type\":\"authentication_error\","
                          "\"message\":\"Cannot authenticate\"}}"))
                 "Cannot authenticate"))
  (should (equal (jev--error-detail "{\"detail\":\"Not Found\"}") "Not Found"))
  (should (equal (jev--error-detail "{\"error\":{\"message\":\"nope\"}}") "nope"))
  (should-not (jev--error-detail "{\"detail\":{\"error_type\":\"x\"}}"))
  (should-not (jev--error-detail "not json")))

(ert-deftest jev-test-an-error-entry-with-only-a-code ()
  "An entry that says nothing must not displace the HTTP status with
the word \"nil\"."
  (should (equal (jev--error-detail "{\"errors\":[{\"code\":2021}]}") "Error code 2021"))
  (should (equal (jev--error-detail "{\"errors\":[{\"message\":\"no\"}]}") "no"))
  (should-not (jev--error-detail "{\"errors\":[{}]}"))
  (let ((jev-api-key "k"))
    (jev-tests--with-stub (make-list (1+ jev-max-retries)
                                     (list :status 502 :headers nil
                                           :body "{\"errors\":[{}]}"))
      (let ((err (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                               :type 'jev-api-error)))
        (should (equal (jev-error-message err) "Jev API returned HTTP 502"))))))

(ert-deftest jev-test-no-stray-docstring-escapes ()
  "A docstring escape is written with two backslashes in the source.

One reads as a bare `=' or `[', which the reader keeps and the
help buffer prints -- `typesafe=' where `typesafe' was meant, and
a literal [customize-variable] where the key binding belongs.
The source is checked rather than any one docstring."
  (let ((directory (file-name-directory (locate-library "jev-core")))
        (stray nil))
    (dolist (name '("jev-core.el" "jev-providers.el" "jev-http.el"
                    "jev-usage.el" "jev.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name name directory))
        (goto-char (point-min))
        (while (re-search-forward "[^\\\\]\\\\[=[]" nil t)
          (push (format "%s:%d" name (line-number-at-pos)) stray))))
    (should-not stray)))

;;;; Edge cases in questions

(ert-deftest jev-test-a-refused-sync-retry-is-a-connection-failure ()
  "A transport can refuse a request instead of answering it.

The first refusal is the caller's to hear -- nothing was sent.
A later one has retries left to spend on it, and spending them is
what the asynchronous path does."
  (let ((jev-api-key "k")
        (jev-retry-initial-delay 0)
        (jev-max-retries 2)
        (attempts 0))
    (let ((jev-http-function
           (lambda (_url _headers _body _timeout _sync _callback)
             (setq attempts (1+ attempts))
             (if (= attempts 1)
                 (list :status 500 :headers nil :body "{}")
               (error "The transport refused to send this")))))
      (should-error (jev-ask-sync "s" (jev-questions (q noul "?")))
                    :type 'jev-connection-error))
    (should (equal attempts 3))))

(ert-deftest jev-test-a-rubric-cannot-say-the-same-thing-twice ()
  "A level written twice is one rung of the rubric, named two ways."
  (should-error (jev-score "How bad?" '("low" "low" "high"))
                :type 'jev-invalid-question)
  ;; A choice already refused this; both now say so the same way.
  (should-error (jev-choice "Which?" '("a" "a")) :type 'jev-invalid-question))

(ert-deftest jev-test-a-noul-has-only-two-sides-to-describe ()
  "A description filed under anything else would never be read."
  (should-error (jev-noul "Urgent?" '(("urgent" . "...")))
                :type 'jev-invalid-question)
  (should-error (jev-noul "Urgent?" '(("true" . "a") (:true . "b")))
                :type 'jev-invalid-question)
  ;; Both sides, one side, or neither.
  (should (jev-question-p (jev-noul "Urgent?" '(("true" . "now") ("false" . "later")))))
  (should (jev-question-p (jev-noul "Urgent?" '(("false" . "later")))))
  (should (jev-question-p (jev-noul "Urgent?"))))

(ert-deftest jev-test-probabilities-are-the-callers-to-keep ()
  "Sorting them in place must not rearrange the answer itself."
  (let ((reply (jev--parse-reply jev-tests--reply nil)))
    (should (equal (mapcar #'car (nreverse (jev-probabilities reply 'team)))
                   '("tech" "billing")))
    (should (equal (mapcar #'car (jev-probabilities reply 'team))
                   '("billing" "tech")))))

(ert-deftest jev-test-a-finished-request-has-nothing-left-to-cancel ()
  "`jev-cancel\\=' must not reach into an attempt that is over."
  (let ((jev-api-key "k")
        (stopped 0))
    (let* ((jev-http-function
            (lambda (_url _headers _body _timeout sync callback)
              (if sync (list :status 200 :headers nil :body "{}")
                (funcall callback (jev-tests--ok jev-tests--reply))
                (lambda () (setq stopped (1+ stopped))))))
           (request (jev-ask "s" (jev-questions (q noul "?")))))
      (jev-cancel request)
      (should (equal stopped 0)))))

(ert-deftest jev-test-a-hash-table-is-checked-like-a-list ()
  "The same options written two ways are checked the same way."
  (should-error (jev-choice "Which?" '(("a" . 42) ("b")))
                :type 'jev-invalid-question)
  ;; A hash table is not a way past that check.
  (should-error (jev-choice "Which?" (let ((h (make-hash-table :test #'equal)))
                                       (puthash "a" 42 h)
                                       (puthash "b" nil h)
                                       h))
                :type 'jev-invalid-question)
  (should (jev-question-p
           (jev-choice "Which?" (let ((h (make-hash-table :test #'equal)))
                                  (puthash "a" "Money" h)
                                  (puthash "b" nil h)
                                  h)))))

(ert-deftest jev-test-an-option-has-to-be-called-something ()
  "An empty label is an option the model cannot tell from no option."
  (should-error (jev-choice "Which?" '("" "b")) :type 'jev-invalid-question)
  (should-error (jev-choice "Which?" '(("  " . "Blank") ("b")))
                :type 'jev-invalid-question)
  (should-error (jev-score "How bad?" '("low" "")) :type 'jev-invalid-question))

(ert-deftest jev-test-a-rubric-may-be-written-in-numbers ()
  "`(1 2 3 4 5)\=' is a rubric, and says what it looks like it says."
  (let ((question (jev-score "How bad?" '(1 2 3 4 5))))
    (should (equal (jev-question-criteria question) '("1" "2" "3" "4" "5"))))
  (should (equal (jev-question-criteria (jev-choice "Which?" '(1 2)))
                 '(("1") ("2"))))
  (should-error (jev-score "How bad?" (list "low" (list "high")))
                :type 'jev-invalid-question))

(ert-deftest jev-test-a-transport-that-answers-twice-is-answered-once ()
  "`jev-http-function\=' is someone else\='s code; SUCCESS runs once."
  (let* ((jev-api-key "k")
         (answers 0)
         (ends 0)
         (jev-request-end-functions
          (list (lambda (_info) (setq ends (1+ ends)))))
         (jev-http-function
          (lambda (_url _headers _body _timeout sync callback)
            (let ((result (jev-tests--ok jev-tests--reply)))
              (if sync result
                (funcall callback result)
                (funcall callback result)
                nil)))))
    (jev-ask "s" (jev-questions (q noul "?"))
             :success (lambda (_reply _tag) (setq answers (1+ answers))))
    (jev-tests--wait-for (lambda () (> answers 0)))
    ;; Long enough for a second delivery to have landed, had one been
    ;; scheduled.
    (sleep-for 0.05)
    (should (equal answers 1))
    (should (equal ends 1))))

;;;; The HTTP client, over a socket

;; Everything above stubs the transport, which is what keeps the suite
;; fast and offline.  The client itself is exercised here against a
;; server the suite runs on the loopback interface, so that what goes
;; down the wire and what comes back up it are both read for real --
;; in one piece and in several, framed every way a server may frame
;; it, and cut short.

(defvar jev-tests--server nil
  "The local server a socket test is answering itself with.")

(defvar jev-tests--served 0
  "How many requests the local server has answered.
A retry arrives on a new connection, so this counts attempts.")

(defvar jev-tests--heard nil
  "Every chunk the local server has received, newest first.")

(defun jev-tests--can-listen-p ()
  "Return non-nil when this machine lets the suite open a local socket."
  (ignore-errors
    (let ((probe (make-network-process :name "jev-tests-probe" :server t
                                       :host 'local :service t :family 'ipv4
                                       :noquery t)))
      (delete-process probe)
      t)))

(defun jev-tests--send-in-pieces (connection pieces)
  "Send PIECES down CONNECTION one at a time, a moment apart, then hang up.
The client may well have gone by then, having read all it needed."
  (ignore-errors
    (if (null pieces)
        (process-send-eof connection)
      (process-send-string connection (car pieces))
      (run-at-time 0.02 nil #'jev-tests--send-in-pieces connection (cdr pieces)))))

(defun jev-tests--serve (response)
  "Listen on a free local port, answering each request with RESPONSE.

RESPONSE is a string, sent whole and followed by a hang-up; a
list of strings, sent one at a time with a pause between them; or
nil, for a server that accepts the request and then says nothing,
which is what a timeout needs to be waited out.  Return the port:
the operating system picks it, so two servers never collide."
  (setq jev-tests--served 0)
  (setq jev-tests--heard nil)
  (setq jev-tests--server
        (make-network-process
         :name "jev-tests-server" :server t :host 'local :service t
         :family 'ipv4 :coding 'binary :noquery t
         :filter (lambda (connection chunk)
                   (push chunk jev-tests--heard)
                   ;; A request may arrive in more than one piece;
                   ;; answer the first and say nothing after that.
                   (unless (process-get connection 'answered)
                     (process-put connection 'answered t)
                     (setq jev-tests--served (1+ jev-tests--served))
                     (cond
                      ((stringp response)
                       (ignore-errors
                         (process-send-string connection response)
                         (process-send-eof connection)))
                      (response
                       (jev-tests--send-in-pieces connection response)))))))
  (cadr (process-contact jev-tests--server)))

(defun jev-tests--request-heard ()
  "Return the whole request the local server received, as text."
  (apply #'concat (reverse jev-tests--heard)))

(defun jev-tests--response (status body)
  "Return a raw HTTP response carrying STATUS and the JSON BODY."
  (format (concat "HTTP/1.1 %d Because\r\n"
                  "Content-Type: application/json\r\n"
                  "x-request-id: req_socket\r\n"
                  "Content-Length: %d\r\n"
                  "Connection: close\r\n"
                  "\r\n%s")
          status (string-bytes body) body))

(defun jev-tests--in-flight ()
  "Return what the client has left open: its processes and its buffers."
  (append
   (seq-filter (lambda (process)
                 (string-match-p "\\`jev\\(<[0-9]+>\\)?\\'" (process-name process)))
               (process-list))
   (seq-filter (lambda (buffer)
                 (string-prefix-p " *jev-http*" (buffer-name buffer)))
               (buffer-list))))

(defmacro jev-tests--with-server (response &rest body)
  "Run BODY against a local server answering RESPONSE.
`jev-base-url' points at it and nothing is stubbed, so the
request goes out through the client and the reply comes back
through it."
  (declare (indent 1))
  `(let ((port (jev-tests--serve ,response)))
     (unwind-protect
         (let ((jev-base-url (format "http://127.0.0.1:%d" port))
               (jev-api-key "test-key")
               (jev-max-retries 0)
               (jev-http-function nil)
               (jev-session-usage nil))
           ,@body)
       (delete-process jev-tests--server))))

(defconst jev-tests--socket-reply
  (json-serialize '((answers (urgent (type . "noul") (noul . 0.9)))
                    (usage (input_tokens . 11))))
  "The body the local server answers a good request with.")

(ert-deftest jev-test-a-real-reply-is-read-over-a-socket ()
  "The answer comes out of the body, the id out of the headers.

And the request that went the other way is HTTP: the method and
the path, the host, the length of the body, and the key."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server (jev-tests--response 200 jev-tests--socket-reply)
    (let ((reply (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))))
      (should (equal (jev-value reply 'urgent) 0.9))
      (should (equal (jev-input-tokens reply) 11))
      (should (equal (jev-reply-request-id reply) "req_socket"))
      (let ((heard (jev-tests--request-heard)))
        (should (string-prefix-p "POST /v1/systemone HTTP/1.1\r\n" heard))
        (should (string-match-p (format "\r\nHost: 127.0.0.1:%d\r\n" port) heard))
        (should (string-match-p "\r\nAuthorization: Bearer test-key\r\n" heard))
        (should (string-match-p "\r\nContent-Type: application/json\r\n" heard))
        (let ((body (substring heard (+ 4 (string-match "\r\n\r\n" heard)))))
          (should (string-match-p (format "\r\nContent-Length: %d\r\n" (length body)) heard))
          (should (equal (alist-get 'model (json-parse-string body :object-type 'alist))
                         jev-default-model)))))
    ;; Once, and nothing left open afterwards.
    (should (equal jev-tests--served 1))
    (should-not (jev-tests--in-flight))))

(ert-deftest jev-test-a-real-reply-is-read-asynchronously ()
  "The same reply, arriving in a callback after `jev-ask' has returned."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server (jev-tests--response 200 jev-tests--socket-reply)
    (let ((seen nil))
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :tag 'ctx
               :success (lambda (reply tag) (setq seen (cons (jev-value reply 'urgent) tag)))
               :error (lambda (err _tag) (setq seen err)))
      (should-not seen)
      (jev-tests--wait-for (lambda () seen))
      (should (equal seen '(0.9 . ctx)))
      (should-not (jev-tests--in-flight)))))

(ert-deftest jev-test-a-reply-that-arrives-in-pieces ()
  "A reply is read whole however the network splits it up.

Here it comes in five pieces, chunked, with the status line, the
headers, a chunk boundary and the last chunk all cut across a
piece.  Nothing is reported until the last one lands, and what
is reported is the whole body."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server
      (list "HTTP/1.1 200 OK\r\nContent-Type: application/js"
            "on\r\nTransfer-Encoding: chunked\r\nx-request-id: req_pieces\r\n\r"
            (format "\n%x\r\n%s" 20 (substring jev-tests--socket-reply 0 20))
            (format "\r\n%x\r\n%s\r\n" (- (length jev-tests--socket-reply) 20)
                    (substring jev-tests--socket-reply 20))
            "0\r\n\r\n")
    (let ((reply (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))))
      (should (equal (jev-value reply 'urgent) 0.9))
      (should (equal (jev-input-tokens reply) 11))
      (should (equal (jev-reply-request-id reply) "req_pieces")))
    (let ((seen nil))
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :success (lambda (reply _tag) (setq seen (jev-value reply 'urgent)))
               :error (lambda (err _tag) (setq seen err)))
      (jev-tests--wait-for (lambda () seen))
      (should (equal seen 0.9)))
    (should-not (jev-tests--in-flight))))

(ert-deftest jev-test-a-reply-that-ends-with-the-connection ()
  "Without a length or chunks, the body is whatever arrives before the hang-up."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server
      (list "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
            (substring jev-tests--socket-reply 0 10)
            (substring jev-tests--socket-reply 10))
    (let ((reply (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))))
      (should (equal (jev-value reply 'urgent) 0.9)))
    (should-not (jev-tests--in-flight))))

(ert-deftest jev-test-a-real-status-is-read-over-a-socket ()
  "A refusal over a socket is the status it was, not a broken connection."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server
      (jev-tests--response 402 (json-serialize '((error (message . "no balance")))))
    (let ((err (should-error (jev-ask-sync "state"
                                           (jev-questions (urgent noul "Urgent?")))
                             :type 'jev-billing-error)))
      (should (equal (jev-error-message err) "no balance"))
      (should (equal (jev-error-status err) 402))
      (should (equal (jev-error-request-id err) "req_socket")))))

(ert-deftest jev-test-a-real-status-keeps-its-type-asynchronously ()
  "A 402 over a socket is a billing error, once.
A refusal the API means is not asked for twice more."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server
      (jev-tests--response 402 (json-serialize '((error (message . "no balance")))))
    (let ((jev-max-retries 2)
          (jev-retry-initial-delay 0)
          (caught nil))
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :error (lambda (err _tag) (setq caught err)))
      (jev-tests--wait-for (lambda () caught))
      ;; Compared as a list, so that a failure says what arrived.
      (should (equal (list (car caught) (jev-error-message caught)
                           (jev-error-status caught) (jev-error-request-id caught))
                     '(jev-billing-error "no balance" 402 "req_socket")))
      (should (equal jev-tests--served 1)))))

(ert-deftest jev-test-a-server-that-hangs-up-is-tried-again ()
  "A connection closed before any answer is a transport failure, and retried."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server ""
    (let ((jev-max-retries 2)
          (jev-retry-initial-delay 0))
      (let ((err (should-error (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))
                               :type 'jev-connection-error)))
        (should-not (eq (car err) 'jev-timeout-error))
        (should (string-match-p "closed the connection" (jev-error-message err))))
      (should (equal jev-tests--served 3))
      (let ((caught nil))
        (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
                 :error (lambda (err _tag) (setq caught err)))
        (jev-tests--wait-for (lambda () caught))
        (should (eq (car caught) 'jev-connection-error))
        (should (equal jev-tests--served 6))))
    (should-not (jev-tests--in-flight))))

(ert-deftest jev-test-a-reply-cut-short-is-tried-again ()
  "A hang-up in the middle of the body is a failure, not the half that arrived."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server
      (concat "HTTP/1.1 200 OK\r\nContent-Length: 500\r\n\r\n" jev-tests--socket-reply)
    (let ((jev-max-retries 1)
          (jev-retry-initial-delay 0))
      (let ((err (should-error (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))
                               :type 'jev-connection-error)))
        (should (string-match-p "mid-response" (jev-error-message err))))
      (should (equal jev-tests--served 2)))
    (should-not (jev-tests--in-flight))))

(defconst jev-tests--redirect-response
  (concat "HTTP/1.1 302 Found\r\n"
          "Location: http://127.0.0.1:1/elsewhere\r\n"
          "Content-Length: 0\r\nConnection: close\r\n\r\n")
  "A refusal to answer that points somewhere else instead.")

(ert-deftest jev-test-a-real-redirect-is-refused-synchronously ()
  "A 3xx is the endpoint answering, not the connection breaking.
Read as a transport failure it would be retried -- asking the
same endpoint to redirect again."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server jev-tests--redirect-response
    (let* ((jev-max-retries 2)
           (jev-retry-initial-delay 0)
           (err (should-error (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))
                              :type 'jev-api-error)))
      (should (equal (list (jev-error-message err) (jev-error-status err) jev-tests--served)
                     (list "The Jev API redirected the request to http://127.0.0.1:1/elsewhere, which is not followed"
                           302 1))))))

(ert-deftest jev-test-a-real-redirect-is-refused-asynchronously ()
  "The same, in a callback."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server jev-tests--redirect-response
    (let ((jev-max-retries 2)
          (jev-retry-initial-delay 0)
          (caught nil))
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :error (lambda (err _tag) (setq caught err)))
      (jev-tests--wait-for (lambda () caught))
      (should (equal (list (car caught) (jev-error-status caught) jev-tests--served)
                     (list 'jev-api-error 302 1)))
      (should (string-match-p "redirected" (or (jev-error-message caught) ""))))))

(ert-deftest jev-test-the-key-never-reaches-a-redirect-target ()
  "A redirect target never hears from this package.

The Authorization header, and the API key in it, would go with a
redirect that was followed.  So this serves a redirect to a
second server and asks that one what it heard: nothing at all."
  (skip-unless (jev-tests--can-listen-p))
  (let* ((heard nil)
         (target (make-network-process
                  :name "jev-tests-target" :server t :host 'local :service t
                  :family 'ipv4 :coding 'binary :noquery t
                  :filter (lambda (connection chunk)
                            (push chunk heard)
                            (process-send-string
                             connection
                             (concat "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n"
                                     "Connection: close\r\n\r\n{}"))
                            (process-send-eof connection))))
         (port (cadr (process-contact target))))
    (unwind-protect
        (jev-tests--with-server
            (format (concat "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:%d/next\r\n"
                            "Content-Length: 0\r\nConnection: close\r\n\r\n")
                    port)
          (let ((jev-api-key "secret-key-that-must-not-travel")
                (jev-max-retries 0)
                (questions (jev-questions (urgent noul "Urgent?")))
                (caught nil))
            (ignore-errors (jev-ask-sync "state" questions))
            (jev-ask "state" questions :error (lambda (err _tag) (setq caught err)))
            (jev-tests--wait-for (lambda () caught))
            (should (eq (car caught) 'jev-api-error))
            (should (equal heard nil))))
      (delete-process target))))

(ert-deftest jev-test-a-redirect-is-an-api-error-and-not-retried ()
  "Where the transport reports a 3xx as the status it is, the layer
above names it, and nobody asks again."
  (let ((result '(:status 307 :headers (("location" . "http://elsewhere")) :body "")))
    (should-not (jev-http--retryable-p result))
    (let ((err (should-error (jev--handle-result result nil 'typesafe "m")
                             :type 'jev-api-error)))
      (should (string-match-p "http://elsewhere" (jev-error-message err)))
      (should (equal (jev-error-status err) 307)))))

(ert-deftest jev-test-a-sync-timeout-leaves-nothing-fetching ()
  "What a synchronous attempt gives up on is taken down with it."
  (skip-unless (jev-tests--can-listen-p))
  ;; A server that accepts and says nothing.
  (jev-tests--with-server nil
    (let ((jev-timeout 0.2))
      (should-error (jev-ask-sync "state" (jev-questions (urgent noul "Urgent?")))
                    :type 'jev-timeout-error)
      (should (equal jev-tests--served 1))
      (should-not (jev-tests--in-flight)))))

(ert-deftest jev-test-an-async-timeout-is-retried-and-leaves-nothing-fetching ()
  "An attempt that times out is stopped, tried again, and then reported."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server nil
    (let ((jev-timeout 0.2)
          (jev-max-retries 1)
          (jev-retry-initial-delay 0)
          (caught nil))
      (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
               :error (lambda (err _tag) (setq caught err)))
      (jev-tests--wait-for (lambda () caught))
      (should (eq (car caught) 'jev-timeout-error))
      (should (equal jev-tests--served 2))
      (should-not (jev-tests--in-flight)))))

(ert-deftest jev-test-a-nil-timeout-waits-and-a-cancel-ends-the-wait ()
  "A nil `jev-timeout' means no timeout, not one that has already passed.

`run-at-time' reads a nil delay as \"now\", which would time every
request out the instant it was sent.  With nothing else to end
the wait, `jev-cancel' does, and takes the connection down."
  (skip-unless (jev-tests--can-listen-p))
  (jev-tests--with-server nil
    (let* ((jev-timeout nil)
           (ends nil)
           (jev-request-end-functions (list (lambda (info) (push info ends))))
           (seen nil)
           (request (jev-ask "state" (jev-questions (urgent noul "Urgent?"))
                             :success (lambda (&rest _) (setq seen 'success))
                             :error (lambda (err _tag) (setq seen err)))))
      (jev-tests--wait-for (lambda () (> jev-tests--served 0)))
      ;; Long enough for a timeout of "now" to have fired.
      (sleep-for 0.1)
      (should-not seen)
      (should (equal (length (jev-tests--in-flight)) 2))
      (should (jev-cancel request))
      (should-not (jev-tests--in-flight))
      (should (equal (mapcar (lambda (info) (car (plist-get info :error))) ends)
                     '(jev-cancelled)))
      (sleep-for 0.05)
      (should-not seen))))

(ert-deftest jev-test-a-quit-still-reaches-the-end-hooks ()
  "C-g is not an `error', and would leave a start hook unbalanced.

Every request that leaves reaches the end hooks exactly once,
which is what makes them safe to count with -- and what a
spinner started in a start hook relies on to ever stop.  The quit
itself still travels to whoever pressed the key."
  (let* ((starts 0) (ends 0) (errors nil)
         (jev-api-key "test-key")
         (jev-request-start-functions (list (lambda (_) (cl-incf starts))))
         (jev-request-end-functions
          (list (lambda (info) (cl-incf ends) (push (plist-get info :error) errors))))
         (jev-http-function (lambda (&rest _) (signal 'quit nil)))
         (questions (jev-questions (urgent noul "Urgent?"))))
    ;; `should-error' catches errors, and a quit is not one of those.
    (should (eq 'quit (condition-case nil
                          (progn (jev-ask-sync "state" questions) 'answered)
                        (quit 'quit))))
    (should (equal (list starts ends) (list 1 1)))
    (setq starts 0 ends 0 errors nil)
    (should (eq 'quit (condition-case nil
                          (progn (jev-ask "state" questions
                                          :success (lambda (&rest _) (ert-fail "success ran"))
                                          :error (lambda (&rest _) (ert-fail "errback ran")))
                                 'sent)
                        (quit 'quit))))
    ;; Nothing is reported through the callbacks: the caller is the
    ;; one who stopped it, and hears the quit.  Long enough for a
    ;; `run-at-time' report to have landed if one had been scheduled.
    (sleep-for 0.1)
    (should (equal (list starts ends) (list 1 1)))
    ;; It was sent, so it is reported as abandoned, once.
    (should (equal (mapcar #'car errors) (list 'jev-cancelled)))))

(provide 'jev-tests)
;;; jev-tests.el ends here
