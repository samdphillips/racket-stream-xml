#lang racket/base

(require (only-in racket/match match)
         xml/spxml/tokenize)

(provide make-sax-parser)

#|
   make-sax-parser : (All (a)
                       (->* ()
                            (#:char-data  (a char-data -> a)
                             #:start-tag  (a start-tag -> a)
                             #:end-tag    (a end-tag -> a)
                             #:comment    (a comment -> a)
                             #:cdata      (a cdata -> a)
                             #:entity-ref (a entity-ref -> a)
                             #:char-ref   (a char-ref -> a)
                             #:pi         (a pi -> a)
                             #:doctype    (a doctype -> a))
                            ((-> (U EOF Xml-Token)) a -> a))
|#

(define (ignore-token doc tok) doc)

(define (make-sax-parser #:char-data    [text-handler       ignore-token]
                         #:start-tag    [start-handler      ignore-token]
                         #:end-tag      [end-handler        ignore-token]
                         #:comment      [comment-handler    ignore-token]
                         #:cdata        [cdata-handler      ignore-token]
                         #:entity-ref   [entity-ref-handler ignore-token]
                         #:char-ref     [char-ref-handler   ignore-token]
                         #:pi           [pi-handler         ignore-token]
                         #:doctype      [doctype-handler    ignore-token])
  (lambda (gen doc)
    (for/fold ([doc doc]) ([tok (in-producer gen eof-object?)])
      (define handler
        (match tok
          [(? char-data?)  text-handler]
          [(? start-tag?)  start-handler]
          [(? end-tag?)    end-handler]
          [(? comment?)    comment-handler]
          [(? cdata?)      cdata-handler]
          [(? entity-ref?) entity-ref-handler]
          [(? char-ref?)   char-ref-handler]
          [(? pi?)         pi-handler]
          [(? doctype?)    doctype-handler]))
      (handler doc tok))))

(module+ test
  (require xml/spxml/well-formed
           (only-in racket/port call-with-input-string))

  (define ((add-on x acc) doc tok)
    (append doc (list (cons x (acc tok)))))

  (define smoke-doc #<<DOC
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

  (call-with-input-string smoke-doc
    (lambda (f)
      (define wf-gen
        (make-wf-generator f))

      (define p
        (make-sax-parser #:start-tag (add-on 'start start-tag-name)
                         #:end-tag   (add-on 'end   end-tag-name)))

      (p wf-gen null)))


  )
