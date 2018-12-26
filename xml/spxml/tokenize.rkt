#lang racket/base

(require (only-in racket/format
                  ~a
                  ~s)
         (only-in racket/match
                  match)
         data/integer-set
         racket/contract
         syntax/srcloc)

(provide
 (contract-out [read-attrs   (-> input-port? (listof attr?))]

               [tokenize-xml (-> input-port? (or/c eof-object?
                                                   xml-token?))]

               [struct xml-token ([location source-location?])
                       #:omit-constructor]

               [struct char-data  ([location source-location?]
                                   [text     string?])]
               [struct start-tag  ([location source-location?]
                                   [closed?  boolean?]
                                   [name     string?]
                                   [attrs    (listof attr?)])]
               [struct end-tag    ([location source-location?]
                                   [name     string?])]
               [struct attr       ([location source-location?]
                                   [name     string?]
                                   [value    (or/c string?
                                                   (listof
                                                     (or/c string?
                                                           entity-ref?)))])]
               [struct comment    ([location source-location?]
                                   [content  string?])]
               [struct cdata      ([location source-location?]
                                   [content  string?])]
               [struct entity-ref ([location source-location?]
                                   [name     string?])]
               [struct char-ref   ([location source-location?]
                                   [value    exact-nonnegative-integer?])]
               [struct pi         ([location source-location?]
                                   [target   string?]
                                   [data     string?])]
               [struct doctype    ([location        source-location?]
                                   [name            string?]
                                   [external-id     (or/c #f)]
                                   [internal-subset (or/c #f string?)])]))

(module+ test
  (require rackunit
           (only-in racket/port call-with-input-string)))

(struct xml-token (location) #:transparent)

(struct char-data  xml-token (text)               #:transparent)
(struct start-tag  xml-token (closed? name attrs) #:transparent)
(struct end-tag    xml-token (name)               #:transparent)
(struct attr       xml-token (name value)         #:transparent)
(struct comment    xml-token (content)            #:transparent)
(struct cdata      xml-token (content)            #:transparent)
(struct entity-ref xml-token (name)               #:transparent)
(struct char-ref   xml-token (value)              #:transparent)
(struct pi         xml-token (target data)        #:transparent)
(struct doctype    xml-token (name
                              external-id
                              internal-subset)    #:transparent)

(define (charset cx)
  (cond
    [(list? cx) (make-range (char->integer (car cx))
                            (char->integer (cadr cx)))]
    [(char? cx) (make-range (char->integer cx))]))

(define (%charset* xs)
  (for/fold ([cs (make-range)]) ([r (in-list xs)])
    (union cs (charset r))))

(define-syntax-rule (charset* x ...)
  (%charset* '(x ...)))

(define name-start-char
  (charset* #\:
            (#\A #\Z)
            #\_
            (#\a #\z)
            (#\uC0 #\uD6)
            (#\uD8 #\uF6)
            (#\uF8 #\u2FF)
            (#\u370 #\u37D)
            (#\u37F #\u1FFF)
            (#\u200C #\u200D)
            (#\u2070 #\u218F)
            (#\u2C00 #\u2FEF)
            (#\u3001 #\uD7FF)
            (#\uF900 #\uFDCF)
            (#\uFDF0 #\uFFFD)
            (#\U10000 #\UEFFFF)))

(define name-char
  (union
   name-start-char
   (charset* #\.
             #\-
             (#\0 #\9)
             #\uB7
             (#\u0300 #\u036F)
             (#\u203F #\u2040))))

(define space-char
  (charset* #\u20
            #\u09
            #\u0d
            #\u0a))

(define pubid-char
  (charset* #\u20
            #\uD
            #\uA
            (#\a #\z)
            (#\A #\Z)
            (#\0 #\9)
            #\- #\( #\) #\+ #\, #\.
            #\/ #\: #\= #\? #\; #\!
            #\* #\# #\@ #\$ #\_ #\%))

(define ((peek-for-charset cs) in [skip 0])
  (define c
    (cond [(string? in) (string-ref in skip)]
          [(and (input-port? in) (zero? skip)) (peek-char in)]
          [else (error 'peek-for-charset "can't read from: ~a" in)]))
  (cond [(eof-object? c) #f]
        [else (member?  (char->integer c) cs)]))

(define-syntax-rule (define-peeker name char-set)
  (define name (procedure-rename (peek-for-charset char-set) 'name)))

(define-peeker name-start-char? name-start-char)
(define-peeker name-char?       name-char)
(define-peeker space-char?      space-char)

(define-syntax-rule (scan v e c ...)
  (let ([v e])
    (and (not (eof-object? v)) c ...)))

(define (peek-string=? s i in)
  (define peek
    (cond [(and (input-port? in) (zero? i)) peek-string]
          [(string? in) (lambda (amount start s)
                          (substring s start (+ start amount)))]
          [else
           (error 'peek-string=? "expected string or input-port got: ~a" in)]))
  (scan v (peek (string-length s) i in) (string=? s v)))

(define (string-index s p*)
  (for/or ([c (in-string s)]
           [i (in-naturals)])
    (for/or ([p (in-list p*)])
      (and (char=? p c) i))))

(define (expect-next name
                     in
                     #:chars [expected-ch* null]
                     #:pred [expected-pred? #f])
  (define pred?
    (or expected-pred?
        (peek-for-charset (%charset* expected-ch*))))
  (unless (pred? in)
    (define expected-name
      (or (and expected-pred? (object-name expected-pred?))
          (~s expected-ch*)))
    (error name "expected one of ~a, got: ~s" expected-name (peek-char in))))

(define (expect-char name expected in)
  (expect-next name in #:chars (list expected))
  (void (read-char in)))

(define (read-until pred? in [start 0])
  (let scan-out ([amount 1024])
    (let ([buf (peek-string amount 0 in)])
      (let scan ([i start])
        (cond
          [(> i amount)  (scan-out (+ amount 1024))]
          [(pred? i buf) (read-string i in)]
          [else (scan (add1 i))])))))

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

(define (read-content in)
  (with-port-location (in source-location)
    (define buf (peek-string 1024 0 in))
    (define i (string-index buf '(#\< #\&)))
    (define (do-read-cdata size)
      (let ([data (read-string size in)])
        (char-data (source-location) data)))
    (cond [(not i)   (do-read-cdata 1024)]
          [(zero? i) (read-special in)]
          [else      (do-read-cdata i)])))

(define (read-special in)
  (define (ref?)
    (scan v (peek-char in) (char=? #\& v)))
  (define (comment?)
    (peek-string=? "<!--" 0 in))
  (define (cdata?)
    (peek-string=? "<![CDATA[" 0 in))
  (define (pi?)
    (peek-string=? "<?" 0 in))
  (define (doctype?)
    (peek-string=? "<!DOCTYPE" 0 in))
  (define (end-tag?)
    (peek-string=? "</" 0 in))
  (define (start-tag?)
    (define s (peek-string 2 0 in))
    (and (char=? #\< (string-ref s 0))
         (name-start-char? s 1)))

  (cond [(start-tag?) (read-start-tag in)]
        [(end-tag?)   (read-end-tag   in)]
        [(comment?)   (read-comment   in)]
        [(ref?)       (read-reference in)]
        [(cdata?)     (read-cdata     in)]
        [(pi?)        (read-pi        in)]
        [(doctype?)   (read-doctype   in)]
        [else
         (error 'read-special
                (~a "expected processing instruction, reference, "
                    "comment, cdata, end tag, or start tag.  got: ~a")
                (peek-string 5 0 in))]))

(define (skip-space in)
  (when (space-char? in)
    (read-char in)
    (skip-space in)))

(define (read-start-tag in)
  (with-port-location (in source-location)
    (read-char in)
    (define name  (read-name in))
    (define attrs (read-attrs in))
    (skip-space in)
    (define closed?
      (cond [(scan v (peek-char in) (char=? #\/ v)) (read-char in) #t]
            [else #f]))
    (expect-char 'read-start-tag #\> in)
    (start-tag (source-location) closed? name attrs)))

(define (read-name in)
  (expect-next 'read-name in #:pred name-start-char?)
  (read-until (lambda (i buf) (not (name-char? buf i))) in 1))

(module+ test
  (check-equal?
   (call-with-input-string "เจมส์ [\r\n<!ELEM" read-name) "เจมส์"))

(define (read-attrs in)
  (for/list ([a (in-port read-attr in)]) a))

(define (read-attr in)
  (skip-space in)
  (cond [(name-start-char? in)
         (with-port-location (in source-location)
           (define name (read-name in))
           (skip-space in)
           (expect-char 'read-attr #\= in)
           (skip-space in)
           (define value (read-attr-value in))
           (attr (source-location) name value))]
        [else eof]))

(define (read-attr-value in)
  (define q (read-char in))
  (unless (or (char=? #\' q) (char=? #\" q))
    (error 'read-attr-value "expected '\"' or \"'\". got: ~a" q))

  (define (read-ref s)
    (read-value (cons (read-reference in) s) 0))

  (define (read-value s [i 1024])
    (define buf (peek-string i 0 in))
    (define-values (c j)
      (cond
        [(eof-object? buf) (values buf #f)]
        [(string-index buf (list q #\< #\&))
         => (λ (j) (values (string-ref buf j) j))]
        [else
         (values #f #f)]))

    (match c
      [(? eof-object?) (error 'read-attr-value
                              "eof while reading attrvalue")]
      [(and (? char?) (? (λ (c) (char=? c q))))
       (normalize (reverse (cons (read-string j in) s)))]
      [#\< (error 'read-attr-value "unescaped '<' in attrvalue")]
      [#\& (read-ref (cons (read-string j in) s))]
      [#f (if (< (string-length buf) i)
              (error 'read-attr-value
                     "eof while reading attrvalue")
              (read-value s (add1 i)))]))

  (define (normalize v)
    (match v
      [(list a)              a]
      [(list a ... "" b ...) (normalize (append a b))]
      [v                     v]))

  (begin0
    (read-value null 0)
    (read-char in)))

(define (read-reference in)
  (with-port-location (in source-location)
    (define (read-char-ref i base)
      (read-string i in)
      (let ([s (read-until-semi)])
        (char-ref (source-location) (string->number s base))))

    (define (read-entity-ref)
      (read-char in)
      (let ([s (read-until-semi)])
        (entity-ref (source-location) s)))

    (define (read-until-semi)
      (begin0
        (read-until (lambda (i buf) (char=? #\; (string-ref buf i))) in)
        (read-char in)))

    (cond [(peek-string=? "&#x" 0 in) (read-char-ref 3 16)]
          [(peek-string=? "&#"  0 in) (read-char-ref 2 10)]
          [else (read-entity-ref)])))


(define (read-end-tag in)
  (with-port-location (in source-location)
    (read-string 2 in)
    (define name (read-name in))
    (skip-space in)
    (expect-char 'read-end-tag #\> in)
    (end-tag (source-location) name)))

(define (read-comment in)
  (with-port-location (in source-location)
    (define (end-comment? i in)
      (peek-string=? "--" i in))

    (read-string 4 in)
    (define content
      (read-until end-comment? in))

    (read-string 2 in)
    (expect-char 'read-comment #\> in)
    (comment (source-location) content)))

(define (read-cdata in)
  (with-port-location (in source-location)
    (define (end-cdata? i in)
      (peek-string=? "]]>" i in))

    ; <!CDATA[[
    (read-string 9 in)
    (define content
      (read-until end-cdata? in))

    (read-string 3 in)
    (cdata (source-location) content)))

(define (read-pi in)
  (with-port-location (in source-location)
    (define (end-pi? i in)
      (peek-string=? "?>" i in))

    (read-string 2 in)
    (define target (read-name in))

    (skip-space in)
    (define data (read-until end-pi? in))
    (read-string 2 in)
    (pi (source-location) target data)))

(define (read-doctype in)
  (with-port-location (in source-location)
    ; <!DOCTYPE
    (read-string 9 in)
    (skip-space in)
    (define doc-name (read-name in))
    (skip-space in)
    (define external-id
      (cond
        [(peek-string=? "SYSTEM" 0 in)
         (read-string 6 in)
         (skip-space in)
         (list 'SYSTEM (read-system-literal in))]
        [(peek-string=? "PUBLIC" 0 in)
         (read-string 6 in)
         (skip-space in)
         (define pubid (read-pubid-literal in))
         (skip-space in)
         (list 'PUBLIC pubid (read-system-literal in))]
        [else #f]))
    (skip-space in)
    (define internal-subset
      (if (scan v (peek-char in 0) (char=? #\[ v))
          (read-internal-subset in)
          #f))
    (skip-space in)
    (expect-char 'read-doctype #\> in)
    (doctype (source-location) doc-name external-id internal-subset)))

(define (read-system-literal in)
  (define q (read-char in))

  (unless (or (char=? #\' q) (char=? #\" q))
    (error 'read-system-literal "expected ' or \" got: ~a" q))

  (begin0
    (read-until (lambda (i buf) (scan v (string-ref buf i) (char=? q v))) in)
    (read-char in)))

(define (read-pubid-literal in) #f)

(define (read-internal-subset in)
  (read-char in)
  (begin0
    (read-until (lambda (i buf) (scan v (string-ref buf i) (char=? #\] v))) in)
    (expect-char 'read-internal-subset #\] in)))

(define (tokenize-xml in)
  (define c (peek-char in))
  (or (and (eof-object? c) c)
      (read-content in)))

(module+ test
  (define-syntax-rule (test-read description input-string expected ...)
    (test-case description
               (call-with-input-string input-string
                 (lambda (in)
                   (check-equal? (read-content in) expected) ...))))

  (test-read "char data" "hello world!" (char-data #f "hello world!"))
  (test-read "start tag" "<test>"       (start-tag #f #f "test" null))
  (test-read "start tag with attributes"
             "<test a='1' b=\"2\">"
             (start-tag #f #f "test" (list (attr #f "a" "1")
                                           (attr #f "b" "2"))))
  (test-read "start tag with empty attributes"
             "<test a='' b=\"\">"
             (start-tag #f #f "test" (list (attr #f "a" "")
                                           (attr #f "b" ""))))

  (test-read "start tag with reference attributes"
             "<test a='A &amp; B' b=\"&#34;\">"
             (start-tag #f #f "test" (list (attr #f "a" (list "A "
                                                        (entity-ref #f "amp")
                                                        " B"))
                                            (attr #f "b" (char-ref #f 34)))))

  (test-read "empty tag"
             "<test/>"
             (start-tag #f #t "test" null))
  (test-read "empty tag"
             "<test />"
             (start-tag #f #t "test" null))

  (test-read "end tag"
             "</test>"
             (end-tag #f "test"))
  (test-read "end tag"
             "</test  >"
             (end-tag #f "test"))

  (test-read "comment"
             "<!-- test -->"
             (comment #f " test "))

  (test-read "cdata section"
             "<![CDATA[ <test> ]]>"
             (cdata #f " <test> "))

  (test-read "entity reference in content"
             "hello &amp; world"
             (char-data #f "hello ")
             (entity-ref #f "amp")
             (char-data #f " world"))

  (test-read "PI"
             "<?xml version=\"1.0\"?>"
             (pi #f "xml" "version=\"1.0\""))

  (define doc #<<DOC
<part number="1976">
    <name>Windscreen Wiper</name>
    <description>
      The Windscreen wiper automatically removes rain from your
      windscreen, if it should happen to splash there.  It has a rubber
      <ref part="1977">blade</ref> which can be ordered separately if
      you need to replace it.
    </description>
</part>
DOC
    )

  (test-case "sample document"
             (define tokens
               (call-with-input-string doc
                 (lambda (in)
                   (for/list ([token (in-port tokenize-xml in)])
                     token))))
             (check-equal?
              tokens
              (list
               (start-tag #f #f "part" (list (attr #f "number" "1976")))
               (char-data #f "\n    ")
               (start-tag #f #f "name" '())
               (char-data #f "Windscreen Wiper")
               (end-tag #f "name")
               (char-data #f "\n    ")
               (start-tag #f #f "description" '())
               (char-data
                #f
                (~a "\n      The Windscreen wiper automatically removes rain "
                    "from your\n      windscreen, if it should happen to "
                    "splash there.  It has a rubber\n      "))
               (start-tag #f #f "ref" (list (attr #f "part" "1977")))
               (char-data #f "blade")
               (end-tag #f "ref")
               (char-data
                #f
                (~a " which can be ordered separately if\n      you need to "
                    "replace it.\n    "))
               (end-tag #f "description")
               (char-data #f "\n")
               (end-tag #f "part"))))
  )
