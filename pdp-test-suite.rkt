#lang racket
(require rackunit)
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
    (pattern t:expr))

  (define-splicing-syntax-class defs+tests
    (pattern (~seq d:definition ...
                   t:test ...)
             #:with defs-stx #'(d ...)
             #:with expr-stx #'(t ...))))


(define-struct pdp-test-suite (defs tests))


(define-syntax (define-pdp-test-suite stx)
  (syntax-parse stx
    [(_ dt:defs+tests ...)
     #'(printf "foo")]))

(define-pdp-test-suite (define x 5) (define "5") (printf "6"))




#;(define-pdp-test-suite name
  "description"
  (define defition . rest) ...
  (test-case ....) ...
  ...
  )

;; this should introduce the identifier `name` to be used for running and printing
;; the resutls of the test suite