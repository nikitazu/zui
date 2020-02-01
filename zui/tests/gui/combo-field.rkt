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
      (init-field [value "failed!"])
      (super-new)

      ; properties

      (define/public (get-value) value)
      (define/public (set-value v)
        (set! value v)
        (send this notify-changes '(get-value)))

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

            (zui test-message% ([label "Test combo-field!"]))

            (zui combo-field% ([#:bind-in 'get-value 'set-value]
                              [label "Tests"]
                              [choices '("failed!" "passed!")]
                              [font test-font]))

            (void 'vertical-pane))

          ; test binding in via model
          (send (zui:get-ui-model) set-value "passed!")

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
