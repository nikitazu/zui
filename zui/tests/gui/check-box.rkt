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
      (init-field [test-passed? #f])
      (super-new)

      ; properties

      (define/public (get-test-passed?) test-passed?)
      (define/public (set-test-passed v)
        (set! test-passed? v)
        (send this notify-changes '(get-test-passed?)))

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

            (zui test-message% ([label "Test check-box!"]))

            (zui check-box% ([#:bind-in 'get-test-passed? 'set-value]
                             [label "Test passed?"]
                             [font test-font]
                             [min-width 300]))

            (void 'vertical-pane))

          ; test binding in via model
          (send (zui:get-ui-model) set-test-passed #t)

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
