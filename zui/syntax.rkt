#lang racket

(require syntax/parse)
(require (prefix-in nc: "notify-changes.rkt"))

;;;; ZUI - Zuev's UI syntax extensions
;;;;

(provide current-ui-element
         current-ui-element-table
         current-ui-element-model
         zui
         get-ui-element
         get-ui-model)

(define current-ui-element (make-parameter #f))
(define current-ui-element-table (make-parameter #f))
(define current-ui-element-model (make-parameter #f))

(define-syntax zui
  (syntax-rules (zui)
    ;
    ; without body
    ;
    [(zui class-id ([key value] ...))
     (zui class-id ([key value] ...) (void))]
    ;
    ; with body - #:id keyword argument
    ;
    [(zui class-id ([#:id id:expr] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (hash-set! (current-ui-element-table)
                     id:expr
                     (current-ui-element))
          body-expr ...)]
    ;
    ; with body - #:bind-in keyword argument
    ;
    [(zui class-id ([#:bind-in bind-in:expr-from bind-in:expr-to] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (let ([model (current-ui-element-model)]
                [element (current-ui-element)])
            (define (bind-data)
              (dynamic-send element
                            bind-in:expr-to
                            (dynamic-send model bind-in:expr-from)))
            (send model add-notify-changes-callback
                  (λ (prop-names)
                    (when (memq bind-in:expr-from prop-names)
                      (bind-data))))
            (bind-data))
          body-expr ...)]
    ;
    ; with body - #:bind-body keyword argument
    ;
    [(zui class-id ([#:bind-body bind-body:expr] kv ...) body-expr ...)
     (zui class-id (kv ...)
          (let ([model (current-ui-element-model)]
                [element (current-ui-element)]
                [table (current-ui-element-table)])
            (define (bind-body)
              (for ([child (send element get-children)])
                (send element delete-child child))
              (parameterize ([current-ui-element-model model]
                             [current-ui-element element]
                             [current-ui-element-table table])
                body-expr ...))
            (send model add-notify-changes-callback
                  (λ (prop-names)
                    (when (memq bind-body:expr prop-names)
                      (bind-body))))
            (bind-body)))]
    ;
    ; with body
    ;
    [(zui class-id ([key value] ...) body-expr ...)
     (let ([ui-instance (new class-id [parent (current-ui-element)] [key value] ...)])
       (parameterize ([current-ui-element ui-instance])
         (begin0 ui-instance
                 body-expr ...)))]))

(define (get-ui-element id)
  (hash-ref (current-ui-element-table)
            id))

(define (get-ui-model)
  (current-ui-element-model))

;; Tests
;;

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (require racket/gui)

  (define my-font
    (make-font #:size 20))
  
  (define test-frame%
    (class frame%
      (super-new [label "Test"]
                 [width 640]
                 [height 480])))

  (define test-model%
    (class (nc:notify-changes-mixin object%)
      (init-field [message "BIND-IN test initialized!"]
                  [items (list "Item 1 failed!"
                               "Item 2 failed!"
                               "Item 3 failed!")])
      (super-new)

      ; properties

      (define/public (get-message) message)
      (define/public (set-message v)
        (set! message v)
        (send this notify-changes '(get-message)))

      (define/public (get-items) items)
      (define/public (set-items v)
        (set! items v)
        (send this notify-changes '(get-items)))

      (void 'test-model)))
  
  (define t
    (test-suite
     "Tests for zui"
     (test-case
      "zui"
      (check-equal? (let ([f (new test-frame%)])
                      (parameterize ([current-ui-element f]
                                     [current-ui-element-table (make-hash)]
                                     [current-ui-element-model (new test-model%)])
                        (zui vertical-pane% ([alignment '(center center)])

                             (zui message% ([label "UI composition test passed!"]
                                            [font my-font]
                                            [auto-resize #t]))

                             (zui message% ([#:id "msg"]
                                            [label "ID test failed!"]
                                            [font my-font]
                                            [auto-resize #t]))

                             (zui message% ([#:bind-in 'get-message 'set-label]
                                            [label "BIND-IN test failed!"]
                                            [font my-font]
                                            [auto-resize #t]))

                             (zui vertical-pane% ([#:bind-body 'get-items]
                                                  [stretchable-height #f])
                                  (for ([item (send (get-ui-model) get-items)])
                                    (zui message% ([label (format "ITER test ~a" item)]
                                                   [font my-font]
                                                   [auto-resize #t]))))

                             (void))
                        ; test access by id
                        (send (get-ui-element "msg") set-label "ID test passed!")
                        ; test binding in via model
                        (send (get-ui-model) set-message "BIND-IN test passed!")
                        ; test binding body via model
                        (send (get-ui-model) set-items
                              '("Item A passed!"
                                "Item B passed!")))
                      (send f show #t)
                      (send f center 'both))
                    (void)))
     (void 't)))
  (run-tests t))
