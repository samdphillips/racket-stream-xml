#lang racket/base

(require syntax/srcloc)
(provide with-port-location)

(define (port-source-location port)
  (and (port-counts-lines? port)
       (let-values ([(line col pos) (port-next-location port)])
         (vector (object-name port) line col pos 0))))

(define-syntax-rule (with-port-location (port srcloc) body ...)
  (let ([srcloc
         (let ([start-location (port-source-location port)])
           (lambda ()
             (and (port-counts-lines? port)
                  (build-source-location-vector
                    start-location (port-source-location port)))))])
    body ...))
