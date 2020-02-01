#lang racket/base

(require (prefix-in nc: "../../notify-changes.rkt")
         "../../syntax.rkt")

;; Control message - Tests
;;

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (require racket/gui)

  (define my-font
    (make-font #:size 20))

  (define test-message%
    (class message%
      (super-new [font my-font]
                 [auto-resize #t])))

  (define test-frame%
    (class frame%
      (super-new [label "Test"]
                 [width 640]
                 [height 480])))

  (define test-model%
    (class (nc:notify-changes-mixin object%)
      (init-field [message "BIND-IN test initialized!"])
      (super-new)

      ; properties

      (define/public (get-message) message)
      (define/public (set-message v)
        (set! message v)
        (send this notify-changes '(get-message)))

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

            (zui test-message% ([label "Test message"]))

            (zui test-message% ([#:bind-in 'get-message 'set-label]
                                [label "BIND-IN test failed!"]))

            (void 'vertical-pane))

          ; test binding in via model
          (send (zui:get-ui-model) set-message "BIND-IN test passed!")

          ; show frame
          (let ([f (zui:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
