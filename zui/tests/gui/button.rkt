#lang racket/base

(require (prefix-in nc: "../../notify-changes.rkt")
         "../../syntax.rkt"
         "private/helpers.rkt")

;; Control button - Tests
;;

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (require racket/gui)

  (define test-model%
    (class (nc:notify-changes-mixin object%)
      (init-field [message "BIND-OUT test initialized!"])
      (super-new)

      ; properties

      (define/public (get-message) message)
      (define/public (set-message v)
        (set! message v)
        (send this notify-changes '(get-message)))

      ; public

      (define/public (change-message s e)
        (send this set-message "BIND-OUT test passed!"))

      (void 'test-model)))

  (define t
    (test-suite
     "Tests for zui"
     (test-case
      "zui"

      ; UI init
      (zui ([#:model (new test-model%)]
            [#:table (make-hash)])

        ; UI markup
        (zui test-frame% ([#:id "frame"])

          (zui vertical-pane% ([alignment '(center center)])

            (zui test-message% ([label "Test button"]))

            (zui test-message% ([#:bind-in 'get-message 'set-label]
                                [label "BIND-IN test failed!"]))

            (zui button% ([#:bind-out 'change-message]
                          [label "Change message"]
                          [font test-font]))

            (void 'vertical-pane))

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
