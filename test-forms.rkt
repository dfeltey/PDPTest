#lang racket

;; This file creates wrappers around Rackunit forms to allow them
;; for use in the automated testing/grading framework.
;; 
;; TODO: 
;;   1. Wrappers for the Rackunit `test` forms
;;   2. A means to record test results to generate grade reports
;;      -- note: the `check` forms may be less useful for this 
;;               purpose

(require (for-syntax syntax/parse
                     racket/syntax)
         (prefix-in ru: rackunit))

(begin-for-syntax
  (define RACKUNIT-PREFIX #'ru:)
  (define-syntax-class bindings
    (pattern ([x e] ...))))

;; This macro does not exactly generate the best possible wrapper
;; It generates syntax from Rackunit functions which is not ideal,
;; but it probably the only way to allow let bindings within test cases
(define-syntax (make-wrapper-form stx)
  (syntax-parse stx 
    [(_ name:id)
     #`(define-syntax (name stx)
         (syntax-parse stx 
           [(_  b:bindings . rest)
            (define orig-name (format-id #'name "~a~a" RACKUNIT-PREFIX #'name))
            #`(let b
                (#,orig-name . rest))]))]))

(define-syntax (define-test-forms/provide stx)
  (syntax-parse stx
    [(_ test ...)
     #'(begin
         (provide test ...)
         (make-wrapper-form test) ...)]))

(define-test-forms/provide 
  check-eq?
  check-not-eq?
  check-eqv?
  check-not-eqv?
  check-equal?
  check-not-equal?
  check-pred
  check-=
  check-true
  check-false
  check-not-false
  check-exn
  check-not-exn
  check-regexp-match
  check
  fail)

