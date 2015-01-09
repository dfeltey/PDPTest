#lang racket

(provide (struct-out pdp-test-case)
         (struct-out pdp-test-suite))
                     
(struct pdp-test-suite (tests))
(struct pdp-test-case (bindings test))