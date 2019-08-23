#lang at-exp racket/base

(require rackunit
         (only-in racket/port call-with-input-string)
         xml/stream/sax
         xml/stream/tokenize
         xml/stream/well-formed)

(call-with-input-string
  @string-append{
<part number="1976">
    <name>Windscreen Wiper</name>
    <description>
      The Windscreen wiper automatically removes rain from your
      windscreen, if it should happen to splash there.  It has a rubber
      <ref part="1977">blade</ref> which can be ordered separately if
      you need to replace it.
    </description>
</part>
  }
  (lambda (f)
    (define wf-gen
      (make-wf-generator
        (lambda () (tokenize-xml f))))

    (define ((add-on x acc) doc tok)
      (append doc (list (cons x (acc tok)))))

    (define p
      (make-sax-parser #:start-tag (add-on 'start start-tag-name)
                       #:end-tag   (add-on 'end   end-tag-name)))

    (check-equal? (p wf-gen null)
                  '((start . "part")
                    (start . "name")
                    (end . "name")
                    (start . "description")
                    (start . "ref")
                    (end . "ref")
                    (end . "description")
                    (end . "part")))))

