#lang racket

(require racket/trait)

;;;; Notify Changes
;;;;

(provide notify-changes-trait
         notify-changes-mixin)

(define notify-changes-trait
  ; todo implement remove-notify-changes-callback
  (trait
   (field [update-callbacks '()])
   
   (define/public (add-notify-changes-callback callback)
     (set! update-callbacks
           (append update-callbacks
                   (list callback))))
   
   (define/public (notify-changes prop-names)
     (for ([callback update-callbacks])
       (callback prop-names)))))

(define notify-changes-mixin
  (trait->mixin notify-changes-trait))


;; Tests
;;

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define test-notificator%
    (class (notify-changes-mixin object%)
      (init-field [message ""])
      (super-new)
      
      ; properties
      (define/public (get-message) message)
      (define/public (set-message v)
        (set! message v)
        (send this notify-changes '(get-message)))
      
      (void 'test-notificator)))
  
  (define t
    (test-suite
     "Tests for notify-changes"
     (test-case
      "notify-changes"
      (check-equal? (let ([n (new test-notificator%)])
                      ; arrange
                      (define changed-properties #f)
                      (send n add-notify-changes-callback
                            (λ (prop-names)
                              (set! changed-properties prop-names)))
                      ; act
                      (send n set-message "Hello!")
                      ; assert
                      changed-properties)
                    '(get-message)))
     (void 't)))
  (run-tests t))
