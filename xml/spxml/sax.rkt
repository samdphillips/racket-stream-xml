#lang racket/base

(require (only-in racket/match
                  match)

         xml/spxml/tokenize)


#|
   make-sax-parser : (All (a)
                       (->* ()
                            (#:char-data  (a char-data -> a)
                             #:start-tag  (a start-tag -> a)
                             #:end-tag    (a end-tag -> a)
                             #:comment    (a comment -> a)
                             #:pi         (a pi -> a))
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
                         #:pi           [pi-handler         ignore-token])
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
          [(? pi?)         pi-handler]))
      (handler doc tok))))

