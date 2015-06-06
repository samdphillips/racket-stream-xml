#lang racket/base

(require (only-in racket/port
                  call-with-input-string)
         racket/generator
         racket/match
         "tokenize.rkt")

(define (string-whitespace? s)
  (for/and ([c (in-string s)])
    (char-whitespace? c)))

(define (valid-xml-decl? tok)
  (define attrs
    (call-with-input-string (pi-data tok) read-attrs))
  (define (check-attr a)
    (match a
      [(or (attr "version" (pregexp "1.\\d+"))
           (attr "standalone" (or "yes" "no"))
           (attr "encoding" _)) #t]
      [_ #f]))
  (for/and ([a (in-list attrs)])
    (check-attr a)))

(define (make-wf-generator in)
  (define (next-token)
    (tokenize-xml in))

  (define (push-token! token)
    (define orig next-token)
    (set! next-token
          (lambda ()
            (set! next-token orig)
            token)))

  (generator ()
    (define (scan-prolog)
      ;; XMLDecl?
      (match (next-token)
        [(and (pi "xml" _) token)
         (unless (valid-xml-decl? token)
           (error 'wfc "invalid xml declaration: ~a~%" token))
         (yield token)]
        [token (push-token! token)])
      ;; Misc*
      (scan-misc*)
      ;; skipping (doctypedecl Misc*)? for now.
      )

    (define (scan-misc*)
      (define (repeat v)
        (yield v)
        (scan-misc*))
      (match (next-token)
        [(and (or (? pi?) (? comment?)) token)
         (repeat token)]
        [(and (char-data (? string-whitespace?)) token)
         (repeat token)]
        [token
         (push-token! token)]))

    (define (scan-match match-name)
      (match (next-token)
        [(and (end-tag name) token)
         (unless (string=? name match-name)
           (error 'wfc
                  "expected end-tag for ~a got: ~s~%"
                  match-name token))
         (yield token)]
        [(and (start-tag #f name _) token)
         (yield token)
         (scan-match name)
         (scan-match match-name)]
        [token
         (yield token)
         (scan-match match-name)]))

    (define (scan-body)
      (match (next-token)
        [(and (start-tag #f name _) token)
         (yield token)
         (scan-match name)]
        [(and (start-tag #t _ _) token)
         (yield token)]
        [token
         (error 'wfc "expected start tag, got: ~a~%" token)]))

    (scan-prolog)
    (scan-body)
    (scan-misc*)

    (match (next-token)
      [(? eof-object? token) (yield token)]
      [token (error 'wfc "expected eof got: ~s~%" token)])))
