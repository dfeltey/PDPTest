#lang racket
(require "test-forms.rkt" "structs.rkt")
(require (for-syntax syntax/parse
                     racket/syntax))
(require racket/pretty)

(define test
  #'(define (foo bar)
      (printf "testing\n") 
      (bar)
      (baz)
      (+ 17 bar)))

(begin-for-syntax
  (define-syntax-class definition
    #:literals (define define-values)
    (pattern (define . rest)
             #:with def-stx #'(define . rest))
    (pattern (define-values . rest)
             #:with def-stx #'(define-values . rest)))
  
  ;; TODO: fill out this test case syntax class
  (define-syntax-class test
    #:literals (test-equal?)
    (pattern (test-equal? . rest)))
  
  ;; the test forms need to be more specific..
  (define-splicing-syntax-class defs+tests
    (pattern (~seq d:definition ...
                   t:test ...)
             #:with defs-stx #'(quote (d ...))
             #:with defs #'(begin d ...)
             #:with expr-stx #'(t ...)
             #:with test-group #'(pdp-test-group (list (quote d) ...)
                                                 (list t ...))
             )))




(define-syntax (define-pdp-test-suite stx)
  (syntax-parse stx
    [(_ name:id dt:defs+tests ...)
     #'(define name
         (pdp-test-suite
         (let ()
           dt.defs ...
           (list dt.test-group ...))))]))

(define-pdp-test-suite t
  (define x 5) 
  (define y "5") 
  (test-equal? "string->number" (string->number y) x) 
  (define z 6)
  (test-equal? "foo" (- z 2) x)
  (define two "two")
  (test-equal? "BAD" (+ two two) 4)
  )




#;(define-pdp-test-suite name
  "description"
  (define defition . rest) ...
  (test-case ....) ...
  ...
  )

;; this should introduce the identifier `name` to be used for running and printing
;; the resutls of the test suite