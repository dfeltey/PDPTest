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
  
  ;; the test forms need to be more specific..
  (define-splicing-syntax-class defs+tests
    (pattern (~seq d:definition ...
                   t:test ...)
             #:with defs-stx #'(quote (d ...))
             #:with defs #'(begin d ...)
             #:with expr-stx #'(t ...))))


(define-struct pdp-test-suite (defs tests))


(define-syntax (define-pdp-test-suite stx)
  (syntax-parse stx
    [(_ dt:defs+tests ...)
     #'(begin (printf "foo\n")
              dt.defs ...
              (list dt.defs-stx ...))
              ]))

(define-pdp-test-suite 
  (define x 5) 
  (define y "5") 
  (printf "6")
  (printf "blah")
  (define w 1)
  (define z 2)
  )




#;(define-pdp-test-suite name
  "description"
  (define defition . rest) ...
  (test-case ....) ...
  ...
  )

;; this should introduce the identifier `name` to be used for running and printing
;; the resutls of the test suite