#lang racket
(require "test-forms.rkt" "structs.rkt")
(require (for-syntax syntax/parse
                     racket/syntax))
(require racket/pretty)

(provide define-pdp-test-suite)

(define test
  #'(define (foo bar)
      (printf "testing\n") 
      (bar)
      (baz)
      (+ 17 bar)))

(begin-for-syntax
  (define-syntax-class definition
    #:literals (define define-values)
    (pattern (define name body)
             #:with def-stx #'(define name body)
             #:with handled-def
             #`(define name (with-handlers ([exn:fail? (lambda (e) (format "Error: Failed to define `~a`" (quote name)))])
                              body))
             )
    #;(pattern (define-values . rest)
             #:with def-stx #'(define-values . rest)))
  
  ;; TODO: fill out this test case syntax class
  (define-syntax-class test
    #:literals (test-equal? test-=)
    (pattern (test-equal? . rest))
    (pattern (test-= . rest))
    (pattern (test-true . rest))
    (pattern (test-false . rest)))
  
  ;; the test forms need to be more specific..
  (define-splicing-syntax-class defs+tests
    (pattern (~seq d:definition ...
                   t:test ...)
             #:with defs-stx #'(quote (d ...))
             #:with defs #'(begin d.handled-def ...)
             #:with expr-stx #'(t ...)
             #:with test-group #'(pdp-test-group (list (quote d) ...)
                                                 (list t ...))
             )))




(define-syntax (define-pdp-test-suite stx)
  (syntax-parse stx
    [(_ name:id dt:defs+tests ...)
     #'(define name
         (pdp-test-suite
          'name
         (let ()
           dt.defs ...
           (list dt.test-group ...))))]))

#;(define-pdp-test-suite t
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