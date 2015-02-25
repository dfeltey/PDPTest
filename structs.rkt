#lang racket

(provide (struct-out pdp-test-case)
         (struct-out pdp-test-suite)
         (struct-out pdp-test-group))

(provide process-pdp-test-suite
         run-pdp-test-suites)

(require rackunit racket/pretty racket/sandbox)

(struct pdp-test-suite (name tests))
(struct pdp-test-group (definitions test-cases))
(struct pdp-test-case (bindings stx test))


(struct pdp-score-result (successes failures errors tests) #:transparent)

(define (print-score-result score-result report-file name)
  (fprintf report-file "Results for Suite ~a:\n" name)
  (fprintf report-file "  Test Successes: ~a\n" (pdp-score-result-successes score-result))
  (fprintf report-file "  Test Failures: ~a\n" (pdp-score-result-failures score-result))
  (fprintf report-file "  Test Errors: ~a\n" (pdp-score-result-errors score-result))
  (fprintf report-file "\nRaw Score: ~a/~a\n"
           (pdp-score-result-successes score-result)
           (pdp-score-result-tests score-result))
  
  (fprintf report-file "Normalized Score: ~a/10\n\n"
           (floor (+ 1/2 (* 10 (/ (pdp-score-result-successes score-result)
                                  (pdp-score-result-tests score-result)))))))

(define (print-overall-score-result score-results report-file)
  (fprintf report-file "Overall Results:\n")
  (fprintf report-file "  Test Successes: ~a\n" (apply + (map pdp-score-result-successes score-results)))
  (fprintf report-file "  Test Failures: ~a\n" (apply + (map pdp-score-result-failures score-results)))
  (fprintf report-file "  Test Errors: ~a\n" (apply + (map pdp-score-result-errors score-results)))
  (fprintf report-file "\nRaw Score: ~a/~a\n"
           (apply + (map pdp-score-result-successes score-results))
           (apply + (map pdp-score-result-tests score-results)))
  
  (fprintf report-file "Normalized Score: ~a/10\n\n"
           (floor (+ 1/2 (* 10 (/ (apply + (map (lambda (sr) (/ (pdp-score-result-successes sr) (pdp-score-result-tests sr))) score-results))
                                  (length score-results)))))))


(define (make-pdp-score-result)
  (pdp-score-result 0 0 0 0))

(define (add-success psr)
  (struct-copy pdp-score-result 
               psr
               [successes (add1 (pdp-score-result-successes psr))]
               [tests (add1 (pdp-score-result-tests psr))]))
(define (add-failure psr)
  (struct-copy pdp-score-result 
               psr
               [failures (add1 (pdp-score-result-failures psr))]
               [tests (add1 (pdp-score-result-tests psr))]))
(define (add-error psr)
  (struct-copy pdp-score-result 
               psr
               [errors (add1 (pdp-score-result-errors psr))]
               [tests (add1 (pdp-score-result-tests psr))]))
(define (add-test psr)
  (struct-copy pdp-score-result 
               psr
               [tests (add1 (pdp-score-result-tests psr))]))

;; better pretty printing for output to test reports
(define (pdp-pretty-print v [output-port (current-output-port)] #:prefix [prefix ""])
  (define pretty-string 
    (with-output-to-string 
     (thunk (pretty-print v (current-output-port) 1))))
  (define pretty-string-list (string-split pretty-string "\n"))
  (for ([str (in-list pretty-string-list)])
    (fprintf output-port "~a" prefix)
    (fprintf output-port "~a" str)
    (fprintf output-port "\n")))


;; run-pdp-test-suites : String pdp-test-suite * -> Void
;; GIVEN: a student identifier (usually their github repository)
;; and a sequence of pdp-test-suites
;; EFFECT: Runs the test suites and generates a test report
(define (run-pdp-test-suites student-id . test-suites)
  (define report-file-name (string-append "pdp-test-results-" student-id ".txt"))
  (define report-file (open-output-file report-file-name #:exists 'replace))
  (fprintf report-file "PDP Test Report for ~a\n\n" student-id)
  (define score-results
    (for/fold ([score-results '() #;(make-pdp-score-result)])
              ([test-suite (in-list test-suites)])
      (append score-results
              (list (process-pdp-test-suite test-suite report-file (make-pdp-score-result) #;score-result)))))
  (fprintf report-file "\n")
  (print-overall-score-result score-results report-file)
  (close-output-port report-file))

;; process-pdp-test-suite : pdp-test-suite output-port -> pdp-score-result
;; GIVEN: a pdp-test-suite and an output port to write a test report to
;; EFFECT: runst the test suite and adds to the report, returns the total points scored
;;         and the number of tests run
(define (process-pdp-test-suite test-suite report-file [score-result (make-pdp-score-result)])
  (define test-groups (pdp-test-suite-tests test-suite))
  (define name (pdp-test-suite-name test-suite))
  (fprintf report-file "\nTest Name: ~a\n" name)
  (for/fold ([score-result score-result])
            ([test-group (in-list test-groups)])
    (define defs (pdp-test-group-definitions test-group))
    (define test-cases (pdp-test-group-test-cases test-group))
    (fprintf report-file "Definitions:\n")
    (for ([def (in-list defs)])
      (pdp-pretty-print def report-file #:prefix "\t"))
    (fprintf report-file "\n")
    (define suite-score-result
      (for/fold ([score-result score-result])
                ([test-case (in-list test-cases)])
        (define stx (pdp-test-case-stx test-case))
        (fprintf report-file "Test Case: \n")
        (pdp-pretty-print stx report-file #:prefix "  ")
        (process/print-test-result test-case report-file score-result)))
    (fprintf report-file "\n")
    (print-score-result suite-score-result report-file name)
    suite-score-result))


(struct out-of-time ())

;; run-pdp-test-case : pdp-test-case -> test-result
(define (run-pdp-test-case ptc)
  (define test (pdp-test-case-test ptc))
  (with-handlers ([exn:fail:resource? (lambda (e) (out-of-time))])
    (with-limits 5 #f 
                 (first (run-test test)))))

;; process/print-test-result : pdp-test-case output-port -> score-result
(define (process/print-test-result ptc report-file score-result)
  (define tr (run-pdp-test-case ptc))
  (cond
    [(test-failure? tr) 
     (fprintf report-file "Test Result: Failure\n")
     (print-check-stack (test-failure-result tr) report-file)
     (fprintf report-file "\n")
     (add-failure score-result)]
    [(test-success? tr)
     (fprintf report-file "Test Result: Success\n\n")
     ;; There is no check stack when processing a success
     ;; Maybe this is ok???
     ;; (print-check-stack (test-success-result tr) report-file)
     (add-success score-result)]
    [(test-error? tr) 
     (fprintf report-file "Test Result: Error\n")
     (print-error-exn (test-error-result tr) report-file)
     (fprintf report-file "\n")
     (add-error score-result)]
    [(out-of-time? tr)
     (fprintf report-file "Test Result: Error out of time\n\n")
     (add-error score-result)]
    [else (printf "Unexpected test-result: ~a\n" tr)
          (error "unexpected test result")]))

(define (print-check-stack result report-file) 
  (define stack (exn:test:check-stack result))
  (define check-stack-info 
    (map (lambda (ci) (list (check-info-name ci) (check-info-value ci)))
         stack))
  (for ([ci (in-list check-stack-info)])
    (unless (member (first ci) (list 'name 'location))
      (fprintf report-file "~a : ~a\n" (first ci) (second ci)))))


(define (print-error-exn result report-file) 
  (fprintf report-file "~a\n" (exn-message result)))

