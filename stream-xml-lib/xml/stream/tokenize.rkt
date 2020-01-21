#lang racket/base

(require (only-in racket/format
                  ~a
                  ~s)
         (only-in racket/match
                  match)
         racket/contract
         syntax/srcloc

         syntax/parse/define

         "private/tokenize/readers.rkt"
         "private/tokenize/srcloc.rkt"
         "private/tokenize/tokens.rkt")

(define attr-value/c
  (or/c string?
        entity-ref?
        char-ref?
        (listof (or/c string?
                      entity-ref?
                      char-ref?))))

(provide
 (contract-out [read-xml-attrs   (-> input-port? (listof attr?))]

               [tokenize-xml (-> input-port? (or/c eof-object?
                                                   xml-token?))]
               [rename tokenize-xml read-xml-token
                       (-> input-port? (or/c eof-object? xml-token?))]

               [use-case-sensitive-doctype? (parameter/c boolean?)]
               [current-xml-special-readers (parameter/c list?)]

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
                                   [value    attr-value/c])]
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
  (require (for-syntax racket/base)
           rackunit
           (only-in racket/match ==)
           (only-in racket/port
                    call-with-input-string
                    with-output-to-string))

  (define-simple-macro (test-read description:string
                                  {~or input-string:string
                                       input-string:expr}
                                  {~optional {~seq #:reader reader}
                                             #:defaults
                                             [(reader #'read-content)]}
                                  expected ...)
    (test-case description
      (call-with-input-string input-string
        (lambda (in)
          (check-match (reader in) expected) ...)))))

(define use-case-sensitive-doctype? (make-parameter #t))

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

(define-simple-macro
  (define/read-special (name:id in:id args:id ...) test:id body ...)
  (define (name in args ...) (and (test in) (let () body ...))))

(define (skip-space in)
  (when (space-char? in)
    (read-char in)
    (skip-space in)))

(define (peek-start-tag? in)
  (define s (peek-string 2 0 in))
  (and (char=? #\< (string-ref s 0))
       (name-start-char? s 1)))

(define/read-special (read-start-tag in)
  peek-start-tag?
  (with-port-location (in source-location)
    (read-char in)
    (define name  (read-name in))
    (define attrs (read-xml-attrs in))
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
  (test-read "read-name special chars"
             "เจมส์ [\r\n<!ELEM"
             #:reader read-name
             "เจมส์"))

(define (read-xml-attrs in)
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

(module+ test
  (test-read "check single entity references are attr-values"
    "'&gt;'"
    #:reader read-attr-value
    (and (entity-ref _ "gt")
         (? attr-value/c)))

  (test-read "check single character references are attr-values"
    "'&#10;'"
    #:reader read-attr-value
    (and (char-ref _ 10)
         (? attr-value/c))))

(define (peek-reference? in)
  (scan v (peek-char in) (char=? #\& v)))

(define/read-special (read-reference in)
  peek-reference?
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

(define (peek-end-tag? in)
  (peek-string=? "</" 0 in))

(define/read-special (read-end-tag in)
  peek-end-tag?
  (with-port-location (in source-location)
    (read-string 2 in)
    (define name (read-name in))
    (skip-space in)
    (expect-char 'read-end-tag #\> in)
    (end-tag (source-location) name)))

(define (peek-comment? in)
  (peek-string=? "<!--" 0 in))

(define/read-special (read-comment in)
  peek-comment?
  (with-port-location (in source-location)
    (define (end-comment? i in)
      (peek-string=? "--" i in))

    (read-string 4 in)
    (define content
      (read-until end-comment? in))

    (read-string 2 in)
    (expect-char 'read-comment #\> in)
    (comment (source-location) content)))

(define (peek-cdata? in)
  (peek-string=? "<![CDATA[" 0 in))

(define/read-special (read-cdata in)
  peek-cdata?
  (with-port-location (in source-location)
    (define (end-cdata? i in)
      (peek-string=? "]]>" i in))

    ; <!CDATA[[
    (read-string 9 in)
    (define content
      (read-until end-cdata? in))

    (read-string 3 in)
    (cdata (source-location) content)))

(module+ test
  (let ([s (with-output-to-string
             (lambda ()
               (for ([n 512]) (display "AbCd"))))])
    (test-read "long cdata"
      (string-append "<![CDATA[" s "]]>")
      #:reader read-cdata
      (cdata _ (== s)))))

(define (peek-pi? in)
  (peek-string=? "<?" 0 in))

(define/read-special (read-pi in)
  peek-pi?
  (with-port-location (in source-location)
    (define (end-pi? i in)
      (peek-string=? "?>" i in))

    (read-string 2 in)
    (define target (read-name in))

    (skip-space in)
    (define data (read-until end-pi? in))
    (read-string 2 in)
    (pi (source-location) target data)))

(define (peek-doctype? in)
  (peek-string=? #:case-sensitive (use-case-sensitive-doctype?)
                  "<!DOCTYPE" 0 in))

(define/read-special (read-doctype in)
  peek-doctype?
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

; FIXME:
(define (read-pubid-literal in) #f)

; FIXME: parse parts of internal subset
(define (read-internal-subset in)
  (read-char in)
  (begin0
    (read-until (lambda (i buf) (scan v (string-ref buf i) (char=? #\] v))) in)
    (expect-char 'read-internal-subset #\] in)))

(define current-xml-special-readers
  (make-parameter
    (list read-start-tag
          read-end-tag
          read-comment
          read-reference
          read-cdata
          read-pi
          read-doctype)))

(define (read-special in)
  (or (for/or ([read (in-list (current-xml-special-readers))]) (read in))
      ;; FIXME: specialize error
      (error 'read-special
             (~a "expected processing instruction, reference, "
                 "comment, cdata, end tag, or start tag.  got: ~a")
             (peek-string 5 0 in))))

(define (tokenize-xml in)
  (define c (peek-char in))
  (or (and (eof-object? c) c)
      (read-content in)))

(module+ test
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

  (test-read "sample document"
    doc
    #:reader tokenize-xml
    (start-tag _ #f "part" (list (attr #f "number" "1976")))
    (char-data _ "\n    ")
    (start-tag _ #f "name" '())
    (char-data _ "Windscreen Wiper")
    (end-tag _ "name")
    (char-data _ "\n    ")
    (start-tag _ #f "description" '())
    (char-data _
               (== (~a "\n      The Windscreen wiper automatically removes rain "
                       "from your\n      windscreen, if it should happen to "
                       "splash there.  It has a rubber\n      ")))
    (start-tag _ #f "ref" (list (attr #f "part" "1977")))
    (char-data _ "blade")
    (end-tag _ "ref")
    (char-data _
               (== (~a " which can be ordered separately if\n      you need to "
                       "replace it.\n    ")))
    (end-tag #f "description")
    (char-data #f "\n")
    (end-tag #f "part")))
