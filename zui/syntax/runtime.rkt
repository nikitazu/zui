#lang racket/base

(require racket/class
         racket/contract
         racket/gui/base)

;;;; Syntax Runtime
;;;;

(provide (contract-out [current-ui-element parameter?]
                       [current-ui-element-table parameter?]
                       [current-ui-element-model parameter?])
         get-ui-element
         get-ui-element*
         get-ui-model
         set-current-ui-element)

;; Parameters
;;

(define current-ui-element (make-parameter #f))
(define current-ui-element-table (make-parameter #f))
(define current-ui-element-model (make-parameter #f))

;; Getters
;;

(define/contract (get-ui-element id)
  (-> string? (is-a?/c area<%>))
  (get-ui-element* id (current-ui-element-table)))

(define/contract (get-ui-element* id ui-element-table)
  (-> string? hash? (is-a?/c area<%>))
  (hash-ref ui-element-table
            id))

(define/contract (get-ui-model)
  (-> object?)
  (current-ui-element-model))

;; Setters
;;

(define/contract (set-current-ui-element id)
  (-> string? any/c)
  (hash-set! (current-ui-element-table)
             id
             (current-ui-element)))

(define/contract (set-ui-element id element)
  (-> string? (is-a?/c area<%>) any/c)
  (set-ui-element* id
                   element
                   (current-ui-element-table)))

(define/contract (set-ui-element* id element ui-element-table)
  (-> string? (is-a?/c area<%>) hash? any/c)
  (hash-set! ui-element-table
             id
             element))

;; Tests
;;


(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (require racket/gui)

  (define test-element (new frame% [label "test"]))
  (define test-model (new object%))

  (define t
    (test-suite
     "Tests for runtime"

     (test-case
      "set-ui-element*/get-ui-element*"
      (let ([table (make-hash)])
        (set-ui-element* "foo" test-element table)
        (check-equal? (get-ui-element* "foo" table)
                      test-element)))

     (test-case
      "set-ui-element/get-ui-element"
      (parameterize ([current-ui-element-table (make-hash)])
        (set-ui-element "foo" test-element)
        (check-equal? (get-ui-element "foo")
                      test-element)))

     (test-case
      "set-current-ui-element"
      (parameterize ([current-ui-element-table (make-hash)]
                     [current-ui-element test-element])
        (set-current-ui-element "foo")
        (check-equal? (get-ui-element "foo")
                      test-element)))

     (test-case
      "get-ui-model"
      (parameterize ([current-ui-element-model test-model])
        (check-equal? (get-ui-model)
                      test-model)))

     (void 't)))
  (run-tests t))
