#lang racket

(require syntax/parse
         (prefix-in nc: "notify-changes.rkt")
         (prefix-in sr: "syntax/runtime.rkt"))

;;;; ZUI - Zuev's UI syntax extensions
;;;;

(provide zui
         (rename-out [sr:current-ui-element zui:current-ui-element]
                     [sr:current-ui-element-table zui:current-ui-element-table]
                     [sr:current-ui-element-model zui:current-ui-element-model]
                     [sr:get-ui-element zui:get-ui-element]
                     [sr:get-ui-element* zui:get-ui-element*]
                     [sr:get-ui-model zui:get-ui-model]))

(define-syntax zui
  (syntax-rules (zui)
    ;
    ; empty
    ;
    [(zui () body-expr ...)
     (begin body-expr ...)]
    ;
    ; #:model keyword argument
    ;
    [(zui ([#:model model-expr] kv ...) body-expr ...)
     (zui (kv ...)
          (parameterize ([sr:current-ui-element-model model-expr])
            body-expr ...))]
    ;
    ; #:table keyword argument
    ;
    [(zui ([#:table table-expr] kv ...) body-expr ...)
     (zui (kv ...)
          (parameterize ([sr:current-ui-element-table table-expr])
            body-expr ...))]
    ;
    ; element without body
    ;
    [(zui class-id ([key value] ...))
     (zui class-id ([key value] ...) (void))]
    ;
    ; element with body - #:id keyword argument
    ;
    [(zui class-id ([#:id id:expr] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (sr:set-current-ui-element id:expr)
          body-expr ...)]
    ;
    ; element with body - #:bind-in keyword argument (with convert)
    ;
    [(zui class-id ([#:bind-in bind-in:expr-from bind-in:expr-to bind-in:convert-func] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (bind-current-ui-element-to-model/in bind-in:expr-from
                                               bind-in:expr-to
                                               bind-in:convert-func)
          body-expr ...)]
    ;
    ; element with body - #:bind-in keyword argument
    ;
    [(zui class-id ([#:bind-in bind-in:expr-from bind-in:expr-to] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (bind-current-ui-element-to-model/in bind-in:expr-from
                                               bind-in:expr-to
                                               values)
          body-expr ...)]
    ;
    ; element with body - #:bind-body keyword argument
    ;
    [(zui class-id ([#:bind-body bind-body:expr] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (bind-current-ui-element-to-model/body bind-body:expr
                                                 (λ () body-expr ...)))]
    ;
    ; element with body
    ;
    [(zui class-id ([key value] ...) body-expr ...)
     (zui-create-ui-element class-id ([key value] ...) body-expr ...)]))


(define (bind-current-ui-element-to-model/in model-getter-id
                                             element-setter-id
                                             convert-func)
  (define model (sr:current-ui-element-model))
  (define element (sr:current-ui-element))

  (define (bind-data)
    (dynamic-send element
                  element-setter-id
                  (convert-func (dynamic-send model model-getter-id))))

  (send model add-notify-changes-callback
        (λ (prop-names)
          (when (memq model-getter-id prop-names)
            (bind-data))))

  (bind-data))


(define (bind-current-ui-element-to-model/body model-getter-id
                                               continuation-func)
  (define model (sr:current-ui-element-model))
  (define element (sr:current-ui-element))
  (define table (sr:current-ui-element-table))

  (define (bind-body)
    (for ([child (send element get-children)])
      (send element delete-child child))
    (parameterize ([sr:current-ui-element-model model]
                   [sr:current-ui-element element]
                   [sr:current-ui-element-table table])
      (continuation-func)))

  (send model add-notify-changes-callback
        (λ (prop-names)
          (when (memq model-getter-id prop-names)
            (bind-body))))

  (bind-body))


(define-syntax-rule (zui-create-ui-element class-id ([key value] ...) body-expr ...)
  (let ([ui-instance (new class-id
                          [parent (sr:current-ui-element)]
                          [key value] ...)])
    (parameterize ([sr:current-ui-element ui-instance])
      (begin0 ui-instance
              body-expr ...))))


;; Tests
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
      (init-field [message "BIND-IN test initialized!"]
                  [count 666]
                  [items (list "Item 1 failed!"
                               "Item 2 failed!"
                               "Item 3 failed!")])
      (super-new)

      ; properties

      (define/public (get-message) message)
      (define/public (set-message v)
        (set! message v)
        (send this notify-changes '(get-message)))

      (define/public (get-count) count)

      (define/public (get-items) items)
      (define/public (set-items v)
        (set! items v)
        (send this notify-changes '(get-items)))

      (void 'test-model)))

  (define (number->string/test n)
    (format "BIND-IN (convert) test passed! (~a)"
            (number->string n)))

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

            (zui test-message% ([label "UI composition test passed!"]))

            (zui test-message% ([#:id "msg"]
                                [label "ID test failed!"]))

            (zui test-message% ([#:bind-in 'get-message 'set-label]
                                [label "BIND-IN test failed!"]))

            (zui test-message% ([#:bind-in 'get-count 'set-label number->string/test]
                                [label "BIND-IN (convert) test failed!"]))

            (zui vertical-pane% ([#:bind-body 'get-items]
                                 [stretchable-height #f])

              (for ([item (send (sr:get-ui-model) get-items)])
                (zui test-message% ([label (format "BIND-BODY test ~a" item)]))))

            (void 'vertical-pane))

          ; test access by id
          (send (sr:get-ui-element "msg") set-label "ID test passed!")

          ; test binding in via model
          (send (sr:get-ui-model) set-message "BIND-IN test passed!")

          ; test binding body via model
          (send (sr:get-ui-model) set-items
                '("Item A passed!"
                  "Item B passed!"))

          ; show frame
          (let ([f (sr:get-ui-element "frame")])
            (send f show #t)
            (send f center 'both))

          (void 'zui)))

      (check-equal? 0 0))
     (void 't)))
  (run-tests t))
