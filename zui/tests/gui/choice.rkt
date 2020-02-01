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
      (init-field [status 'init])
      (super-new)

      ; properties

      (define/public (get-status) status)
      (define/public (set-status v)
        (set! status v)
        (send this notify-changes '(get-status)))

      (void 'test-model)))

  (define (status->selection s)
    (case s
      ['init 0]
      ['failed 1]
      ['passed 2]))

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

            (zui test-message% ([label "Test choice!"]))

            (zui choice% ([#:bind-in 'get-status 'set-selection status->selection]
                          [label "Test  "]
                          [choices '("init..." "failed!" "passed!")]
                          [selection 1]
                          [font test-font]))

            (void 'vertical-pane))

          ; test binding in via model
          (send (zui:get-ui-model) set-status 'passed)

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
