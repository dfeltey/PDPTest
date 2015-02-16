#lang racket

(provide pdp-require pdp-require/bsl)

(require (for-syntax syntax/parse racket/syntax))
(require (only-in rackunit dynamic-require/expose))

(define (dynamic-require/expose/safe mod name)
  ;; if name is undefined, inner function in exn handler is the require result
  (with-handlers ([exn:fail:syntax?
                   (λ _ (λ _ (error (format "function or var ~a undefined" name))))]
                  [exn:fail:contract:variable? 
                   (λ _ (λ _ (error (format "function ~a undefined" name))))]
                  [exn:fail:filesystem? 
                   (λ _ (λ _ (error (format "module ~a does not exist" mod))))])
    (dynamic-require/expose mod name)))

(begin-for-syntax
  (define-syntax-class pdp-require-form
    #:literals (prefix-in)
    (pattern (pdp-require mod-exp fn:id ...+) 
             #:with mod #'mod-exp
             #:with req-ids #'((quote fn) ...)
             #:with def-ids #'(fn ...))
    (pattern (pdp-require (prefix-in pf:id mod-exp fn:id ...+))
             #:with mod #'mod-exp
             #:with req-ids #'((quote fn) ...)
             #:with def-ids (map (lambda (id) (format-id id "~a~a" #'pf id)) 
                                 (syntax->list #'(fn ...))))))

(define-syntax (pdp-require stx)
  (syntax-parse stx 
    [prf:pdp-require-form
     (with-syntax ([(def ...) #'prf.def-ids]
                   [(req ...) #'prf.req-ids])
       #'(begin
           (define def (dynamic-require/expose/safe prf.mod req)) ...))]))

(define-syntax (pdp-require/bsl stx)
  (syntax-parse stx
    [(_ file fn:id ...)
     #`(begin
         (require file)
         (check-binding fn) ...)]))

(define-syntax (check-binding stx)
  (syntax-parse stx
    [(_ fn:id)
     (if (identifier-binding #'fn)
         #'(begin)
         #'(define (fn . args) (error 'pdp-require "~a not provided" (quote fn))))]))
