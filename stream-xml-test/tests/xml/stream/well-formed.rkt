#lang racket/base

(require (only-in racket/port
                  call-with-input-string)
         rackunit

         xml/stream/tokenize
         xml/stream/well-formed)

(define-syntax-rule (check-process/exn name s pats ...)
  (test-case name
    (call-with-input-string s
      (lambda (in)
        (define next (make-wf-generator (lambda () (tokenize-xml in))))
        (check-match (next) pats) ...
        (check-exn exn:fail?
                   (lambda () (next)))))))

(define-syntax-rule (check-process name s pats ...)
  (test-case name
    (call-with-input-string s
      (lambda (in)
        (define next (make-wf-generator (lambda () (tokenize-xml in))))
        (check-match (next) pats) ...
        (check-match (next) (? eof-object?))))))

(check-process/exn "wfc: document production"
                   "<?xml version='1.0' encoding='utf-8'?>  a <test></test>"
                   (pi _ "xml" "version='1.0' encoding='utf-8'"))
(check-process/exn "wfc: document production"
                   "<test>"
                   (start-tag _ #f "test" _))
(check-process/exn "wfc: document production"
                   "<test><test>"
                   (start-tag _ #f "test" _)
                   (start-tag _ #f "test" _))

(check-process/exn "wfc: unique attributes"
                   "<test a=\"1\" b=\"2\" a=\"3\">")
(check-process/exn "wfc: unique attributes"
                   "<test><test a=\"1\" b=\"2\" a=\"3\">"
                   (start-tag _ _ "test" _))

(check-process "doc1"
               "<test><a/></test>"
               (start-tag _ #f "test" null)
               (start-tag _ #t "a" null)
               (end-tag _ "test"))
