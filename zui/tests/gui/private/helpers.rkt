#lang racket/gui

(provide test-font
         test-message%
         test-frame%)

(define test-font
  (make-font #:size 20))

(define test-message%
  (class message%
    (super-new [font test-font]
               [auto-resize #t])))

(define test-frame%
  (class frame%
    (super-new [label "Test"]
               [width 640]
               [height 480])))
