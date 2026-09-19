# jev.el

An Emacs Lisp client for [Jev](https://docs.typesafe.ai/), TypeSafe AI's
System One model. Jev takes unstructured state and typed questions, and answers
each question with a typed value and a calibrated confidence in one round trip.

```elisp
(require 'jev)
(setq jev-api-key "sk-...")   ; or TYPESAFE_API_KEY, or auth-source

(jev-ask "I was charged twice this month and support never replied."
         (jev-questions
          (team     choice "Which team should handle this?"
                    '(("billing" . "Payments and invoices")
                      ("tech"    . "Bugs and outages")))
          (urgent   noul   "Does this convey urgency?")
          (severity score  "How severe is it?" '("cosmetic" "annoying" "blocking")))
         :tag 'ticket-42
         :success (lambda (reply tag)
                    (message "%s: %s (confidence %s), urgent=%s severity=%s"
                             tag
                             (jev-value reply 'team)
                             (or (jev-confidence reply 'team) "-")
                             (jev-true-p reply 'urgent)
                             (jev-level reply 'severity))))
```

## Installation

Emacs 27.1 or later with native JSON support (every Emacs 29+, or a 27/28 built
against libjansson). No other dependencies. Put the files on your `load-path`
and `(require 'jev)`.

## Providers

| | Endpoint | Key |
|---|---|---|
| `typesafe` (default) | `api.typesafe.ai/v1/systemone` | `TYPESAFE_API_KEY` |
| `vercel` | `ai-gateway.vercel.sh/v4/ai/evaluation-model` | `AI_GATEWAY_API_KEY` |

```elisp
(setq jev-provider 'vercel)   ; or M-x jev-use-provider
```

Requests never follow a redirect, and a certificate that does not verify fails
the request. Proxies are not supported by the built-in transport; replace it
through `jev-http-function` if you need one.

## API key

`jev-api-key` takes a string, a function of the provider symbol, or an alist
keyed by provider. Left nil, the key comes from the provider's environment
variable, then from auth-source by endpoint host:

```
# ~/.authinfo.gpg
machine api.typesafe.ai       login jev password sk-...
machine ai-gateway.vercel.sh  login jev password vck_...
```

```elisp
(add-to-list 'auth-sources 'macos-keychain-internet)  ; macOS Keychain
(setq jev-auth-source-user "jev")                     ; when one host holds several secrets
```

## Questions

```elisp
(jev-noul   "Is this a security issue?")
(jev-choice "Which team?" '(("billing" . "Money") ("tech")))   ; 2–255 options, unordered
(jev-score  "How severe?" '("cosmetic" "annoying" "blocking")) ; 2–10 levels, ordered rubric
```

Labels may be strings, symbols or numbers, must be distinct, and must not be
blank. An option may carry a description as `(LABEL . DESCRIPTION)`. A noul's
optional criteria describe only `"true"` and `"false"`. An invalid question
signals `jev-invalid-question` where it is written.

`jev-questions` builds an alist of `(KEY . QUESTION)`; writing the pairs by hand
works too. Keys must be distinct. All questions in one call are answered in one
round trip.

## Asking

- `(jev-ask STATE QUESTIONS &key success error tag model)` — asynchronous.
  Returns a `jev-request`. `:success` is called with the reply and `tag`,
  `:error` with the error object and `tag`. Every failure reaches `:error`;
  nothing is signalled to the caller. Neither callback runs before `jev-ask`
  has returned.
- `(jev-ask-sync STATE QUESTIONS &key tag model)` — blocks and returns the
  reply, or signals a `jev-error`.

`STATE` is a string or any JSON-encodable value: `nil` is `null`
(`jev-empty-object` is `{}`), an alist or plist is an object, any other list
or vector is an array, `t` and `:false` are the booleans.

Connection failures, timeouts and HTTP 408, 429 and 5xx are retried
`jev-max-retries` times with exponential backoff; a `Retry-After` header is
honoured, up to a minute. Each attempt is bounded by `jev-timeout` (nil waits
indefinitely).

## Cancelling

```elisp
(defvar-local my-request nil)
(setq my-request (jev-ask state questions :success #'my-handler))
(add-hook 'kill-buffer-hook (lambda () (jev-cancel my-request)) nil t)
```

After `jev-cancel`, neither callback runs. The end hooks still see the request
once, with an error of `cancelled`.

## Reading a reply

```elisp
(jev-value reply 'team)          ; "billing" — a probability for a noul, a number for a score
(jev-confidence reply 'team)     ; 0.93, or nil where the provider sends none
(jev-probabilities reply 'team)  ; (("billing" . 0.93) ("tech" . 0.07))
(jev-top-choices reply 'team 3)  ; the same, likeliest first, at most three
(jev-true-p reply 'urgent 0.8)   ; threshold defaults to 0.5
(jev-legend reply 'severity)     ; ("cosmetic" "annoying" "blocking")
(jev-level reply 'severity)      ; "blocking"
(jev-input-tokens reply)         ; 442
(jev-output-tokens reply)        ; 68
(jev-usage reply)                ; the raw usage alist
(jev-reply-raw reply)            ; the whole decoded response
```

A score is a weighted position on the rubric: a four-level question answers
between 0.0 and 3.0, with probabilities keyed by level index. `jev-level`
rounds it to the nearest label.

## Errors

```
jev-error
|- jev-configuration-error
|- jev-invalid-question
|- jev-invalid-state        the state cannot be encoded
|- jev-response-error
|- jev-cancelled            reaches the end hooks only
|- jev-connection-error
|  `- jev-timeout-error
`- jev-api-error            any other status, 3xx included
   |- jev-auth-error        401, 403
   |- jev-billing-error     402
   |- jev-validation-error  422
   |- jev-rate-limit-error  429
   `- jev-overloaded-error  529
```

`jev-error-message`, `jev-error-status`, `jev-error-request-id` and
`jev-error-body` read the error object. An error raised by your own callback
is reported with `message` and does not affect the request.

## Cost

```elisp
M-x jev-usage-report        ; jev: 12 requests, 8431 input tokens, about $0.000354
C-u M-x jev-usage-report    ; per provider, in a buffer
M-x jev-usage-reset

(jev-cost reply)                  ; what one reply cost, at list price
(jev-estimate-cost state 100)     ; before a command fans out
```

`jev-cost` uses the price the provider reports, falling back to
`jev-input-token-price` (check it against the [current list
price](https://docs.typesafe.ai/models)). Set `jev-track-usage` to nil to keep
no tally.

## Hooks

```elisp
(add-hook 'jev-request-end-functions
          (lambda (info) (message "jev %s in %.1fs"
                                  (plist-get info :status)
                                  (plist-get info :duration))))
```

`jev-request-start-functions` receives `:url`, `:provider`, `:model` and
`:questions`. `jev-request-end-functions` receives `:url`, `:provider`,
`:status`, `:duration`, `:usage`, `:input-tokens`, `:output-tokens`, `:cost`,
`:request-id` and `:error`. Every request that is sent reaches the end hooks
exactly once, whether it answered, failed or was cancelled. A start hook that
signals stops the request; an end hook that signals is reported and skipped.

## Tests

```sh
make compile test
```

The suite stubs the transport and never reaches the internet; the HTTP client
is tested against a server the suite runs on the loopback interface.

## License

GPL-3.0-or-later.
