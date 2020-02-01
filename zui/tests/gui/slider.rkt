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
      (init-field [value 0])
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

            (zui test-message% ([label "Test slider!"]))

            (zui slider% ([#:bind-in 'get-value 'set-value]
                          [label "Tests passed:"]
                          [min-value 00]
                          [max-value 99]
                          [font test-font]))

            (void 'vertical-pane))

          ; test binding in via model
          (send (zui:get-ui-model) set-value 99)

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
