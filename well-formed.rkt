#lang racket/base

(require (only-in racket/port
                  call-with-input-string)
         (only-in racket/set
                  set
                  set-add
                  set-member?)
         racket/generator
         racket/match
         "tokenize.rkt")

(define wfc-error
  (case-lambda
    [(msg expected actual)
     (error 'wfc "~a. expected: ~a, got: ~s~%" msg expected actual)]
    [(msg value)
     (error 'wfc "~a. ~s~%" msg value)]))

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

(define (unique-attributes? attrs)
  (let/ec ret
    (for/fold ([seen? (set)]) ([a (in-list attrs)])
      (define name (attr-name a))
      (when (set-member? seen? name)
        (ret #f))
      (set-add seen? name))
    #t))

;; DTD TODO WFC (either here or in dtd related processor):
;; [WFC: PE Between Declarations]
;; [WFC: External Subset]
;; [WFC: PEs in Internal Subset]
;;	[WFC: No < in Attribute Values]
;; [WFC: No External Entity References]

;; Entity WFC TODO:
;;   [WFC: Entity Declared]
;;   [VC: Entity Declared]
;;   [WFC: Parsed Entity]
;;   [WFC: No Recursion]
;;   [WFC: In DTD]

;; make-wf-generator : Input-Port -> (-> Xml-Token)
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

    ;; TODO: [WFC: Legal Character]
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

    ;; [WFC: Element Type Match]
    ;; [WFC: Unique Att Spec]
    ;; TODO: [WFC: No External Entity References]
    ;;       [WFC: No < in Attribute Values]
    ;;       [WFC: Legal Character]
    (define (scan-match match-name)
      (match (next-token)
        [(and (end-tag name) token)
         (unless (string=? name match-name)
           (wfc-error "element type match" match-name name))
         (yield token)]
        [(start-tag _ _ attrs) (=> continue)
         (unless (unique-attributes? attrs)
           (wfc-error "unique attributes specified" attrs))
         (continue)]
        [(and (start-tag #f name attrs) token)
         (yield token)
         (scan-match name)
         (scan-match match-name)]
        [(and (start-tag #t _ attrs) token)
         (yield token)]
        [(? eof-object?)
         (wfc-error "document production violation" match-name eof)]
        [token
         (yield token)
         (scan-match match-name)]))

    ;; [WFC: Unique Att Spec]
    ;; [WFC: Single root element]
    ;; TODO: [WFC: No External Entity References]
    ;;       [WFC: No < in Attribute Values]
    (define (scan-body)
      (match (next-token)
        [(start-tag _ _ attrs) (=> continue)
         (unless (unique-attributes? attrs)
           (wfc-error "unique attributes specified" attrs))
         (continue)]
        [(and (start-tag #f name attrs) token)
         (yield token)
         (scan-match name)]
        [(and (start-tag #t _ attrs) token)
         (yield token)]
        [token
         (error 'wfc "expected start tag, got: ~a~%" token)]))

    (scan-prolog)
    (scan-body)
    (scan-misc*)

    (match (next-token)
      [(? eof-object? token) (yield token)]
      [token (error 'wfc "expected eof got: ~s~%" token)])))


(module+ test
  (require rackunit)

  (define-syntax-rule (check-process/exn name s pats ...)
    (test-case name
      (call-with-input-string s
        (lambda (in)
          (define next (make-wf-generator in))
          (check-match (next) pats) ...
          (check-exn exn:fail?
                     (lambda () (next)))))))

  (check-process/exn "wfc: document production"
                     "<?xml version='1.0' encoding='utf-8'?>  a <test></test>"
                     (pi "xml" "version='1.0' encoding='utf-8'"))
  (check-process/exn "wfc: document production"
                     "<test>"
                     (start-tag #f "test" _))
  (check-process/exn "wfc: document production"
                     "<test><test>"
                     (start-tag #f "test" _)
                     (start-tag #f "test" _))

  (check-process/exn "wfc: unique attributes"
                     "<test a=\"1\" b=\"2\" a=\"3\">")
  (check-process/exn "wfc: unique attributes"
                     "<test><test a=\"1\" b=\"2\" a=\"3\">"
                     (start-tag _ "test" _))

)