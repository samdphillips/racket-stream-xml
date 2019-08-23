#lang racket/base

(provide (all-defined-out))

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
