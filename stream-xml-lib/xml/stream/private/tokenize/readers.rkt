#lang racket/base

(require racket/format
         racket/match
         data/integer-set
         syntax/srcloc)

(module+ test
  (require rackunit
           (only-in racket/match ==)
           (only-in racket/port
                    call-with-input-string
                    with-output-to-string)))

(provide scan

         expect-char
         expect-next
         name-char?
         name-start-char?
         peek-string=?
         read-until
         space-char?
         string-index)

(define (charset cx)
  (cond
    [(list? cx) (make-range (char->integer (car cx))
                            (char->integer (cadr cx)))]
    [(char? cx) (make-range (char->integer cx))]))

(define (charset* . xs)
  (for/fold ([cs (make-range)]) ([r (in-list xs)])
    (union cs (charset r))))

(define ((peek-for-charset cs) in [skip 0])
  (define c
    (cond [(string? in) (string-ref in skip)]
          [(and (input-port? in) (zero? skip)) (peek-char in)]
          [else (error 'peek-for-charset "can't read from: ~a" in)]))
  (cond [(eof-object? c) #f]
        [else (member?  (char->integer c) cs)]))

(define-syntax-rule (define-peeker name char-set)
  (define name (procedure-rename (peek-for-charset char-set) 'name)))

(define name-start-char
  (charset* #\:
            '(#\A #\Z)
            #\_
            '(#\a #\z)
            '(#\uC0 #\uD6)
            '(#\uD8 #\uF6)
            '(#\uF8 #\u2FF)
            '(#\u370 #\u37D)
            '(#\u37F #\u1FFF)
            '(#\u200C #\u200D)
            '(#\u2070 #\u218F)
            '(#\u2C00 #\u2FEF)
            '(#\u3001 #\uD7FF)
            '(#\uF900 #\uFDCF)
            '(#\uFDF0 #\uFFFD)
            '(#\U10000 #\UEFFFF)))

(define name-char
  (union
   name-start-char
   (charset* #\.
             #\-
             '(#\0 #\9)
             #\uB7
             '(#\u0300 #\u036F)
             '(#\u203F #\u2040))))

(define space-char
  (charset* #\u20 #\u09 #\u0d #\u0a))

(define pubid-char
  (charset* #\u20 #\uD #\uA
            '(#\a #\z)
            '(#\A #\Z)
            '(#\0 #\9)
            #\- #\( #\) #\+ #\, #\.
            #\/ #\: #\= #\? #\; #\!
            #\* #\# #\@ #\$ #\_ #\%))

(define-peeker name-start-char? name-start-char)
(define-peeker name-char?       name-char)
(define-peeker space-char?      space-char)

(define-syntax-rule (scan v e c ...)
  (let ([v e])
    (and (not (eof-object? v)) c ...)))

(define (peek-string=? s i in #:case-sensitive [case-sensitive? #t])
  (define peek
    (cond [(and (input-port? in) (zero? i)) peek-string]
          [(string? in) (lambda (amount start s)
                          (let* ([end (+ start amount)]
                                 [end
                                   (let ([len (string-length s)])
                                     (if (< end len) end len))])
                            (substring s start end)))]
          [else
           (error 'peek-string=? "expected string or input-port got: ~a" in)]))
  (define =? (if case-sensitive? string=? string-ci=?))
  (scan v (peek (string-length s) i in) (=? s v)))

(module+ test
  (check-not-exn
    (lambda ()
      (peek-string=? "abc" 0 "a"))))

(define (string-index s p*)
  (for/or ([c (in-string s)]
           [i (in-naturals)])
    (for/or ([p (in-list p*)])
      (and (char=? p c) i))))

(define (read-until pred? in [start 0])
  (let scan-out ([amount 1024])
    (let ([buf (peek-string amount 0 in)])
      (let scan ([i start])
        (cond
          [(> i amount)  (scan-out (+ amount 1024))]
          [(pred? i buf) (read-string i in)]
          [else (scan (add1 i))])))))

(define (expect-next name
                     in
                     #:chars [expected-ch* null]
                     #:pred [expected-pred? #f])
  (define pred?
    (or expected-pred?
        (peek-for-charset (apply charset* expected-ch*))))
  (unless (pred? in)
    (define expected-name
      (or (and expected-pred? (object-name expected-pred?))
          (~s expected-ch*)))
    (error name "expected one of ~a, got: ~s" expected-name (peek-char in))))

(define (expect-char name expected in)
  (expect-next name in #:chars (list expected))
  (void (read-char in)))
