#lang racket

(provide pdp-require)

(require (for-syntax syntax/parse
                     racket/syntax))
(require (only-in rackunit dynamic-require/expose))

(begin-for-syntax 
  (define-syntax-class pdp-require-form
    (pattern name:id
             #:with sym #'(quote name)
             #:with def #'name)
    (pattern (orig-name:id new-name:id)
             #:with sym #'(quote orig-name)
             #:with def #'new-name))
  
  (define-splicing-syntax-class keywords
    (pattern 
     (~seq 
      (~or (~optional (~seq #:default de:expr)
                      #:name "#:default option"
                      #:defaults ([de (lambda (name)
                                        #`(error 'pdp-require "~a not provided" (quote #,name)))]))
           (~optional (~seq #:prefix pf:id)
                      #:name "#:prefix option"
                      #:defaults ([pf ""]))) ...)
     #:attr apply-prefix (lambda (stx name) (format-id stx "~a~a" (attribute pf) name))
     #:attr default-value #'de)))

;; NOTE: This should mostly work, an exception is thrown when the "required" module
;;            does not exist, which should be a favorable behavior.
;;
;; TODO: 
;;       1. Better handling of the default values in order to have good error messages
(define-syntax (pdp-require stx)
  (syntax-parse stx
    [(_ mod name:pdp-require-form ... kw:keywords)
     (define apply-prefix (attribute kw.apply-prefix))
     (define default-value (attribute kw.default-value))
     #`(begin
         #,@(for/list ([def-name (syntax->list #'(name.def ...))]
                       [req-name (syntax->list #'(name.sym ...))])
              (define name (apply-prefix stx def-name))
              #`(define #,name 
                  (with-handlers ([exn:fail:contract:variable? (lambda (exn) #,default-value)])
                    (dynamic-require/expose mod #,req-name)))))]))
    
