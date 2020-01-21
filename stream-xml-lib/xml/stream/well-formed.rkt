#lang racket/base

(require (only-in racket/port
                  call-with-input-string)
         (only-in racket/set
                  set
                  set-add
                  set-member?)
         racket/contract
         racket/generator
         racket/match

         xml/stream/tokenize)

(provide
 (contract-out [make-wf-generator
                (-> (-> (or/c eof-object? xml-token?))
                    (-> (or/c eof-object? xml-token?)))]))

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
    (call-with-input-string (pi-data tok) read-xml-attrs))
  (define (check-attr a)
    (match a
      [(or (attr _ "version" (pregexp "1.\\d+"))
           (attr _ "standalone" (or "yes" "no"))
           (attr _ "encoding" _)) #t]
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

;; make-wf-generator : (-> Xml-Token) -> (-> Xml-Token)
(define (make-wf-generator next-token)
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
        [(and (pi _ "xml" _) token)
         (unless (valid-xml-decl? token)
           (error 'wfc "invalid xml declaration: ~a~%" token))
         (yield token)]
        [token (push-token! token)])
      ;; Misc*
      (scan-misc*)

      ;; doctypedecl?
      (match (next-token)
        [(? doctype? token) (yield token)]
        [token (push-token! token)])

      ;; Misc*
      (scan-misc*))

    ;; TODO: [WFC: Legal Character]
    (define (scan-misc*)
      (define (repeat v)
        (yield v)
        (scan-misc*))
      (match (next-token)
        [(and (or (? pi?) (? comment?)) token)
         (repeat token)]
        [(and (char-data _ (? string-whitespace?)) token)
         (repeat token)]
        [token
         (push-token! token)]))

    ;; [WFC: Element Type Match]
    ;; TODO: [WFC: No External Entity References]
    ;;       [WFC: Legal Character]
    (define (scan-match match-name)
      (scan-body (lambda ()
                   (scan-match match-name))
                 (lambda ()
                   (wfc-error "document production violation" match-name eof))
                 (lambda (name token continue)
                   (unless (string=? name match-name)
                     (wfc-error "element type match" match-name name))
                   (yield token))
                 (lambda (token)
                   (yield token)
                   (scan-match match-name))))

    ;; [WFC: Unique Att Spec]
    ;; TODO: [WFC: No External Entity References]
    ;;       [WFC: No < in Attribute Values]
    (define (scan-body kont handle-eof handle-end-tag handle-other-token)
      (match (next-token)
        [(start-tag _ _ _ attrs) (=> continue)
         (unless (unique-attributes? attrs)
           (wfc-error "unique attributes specified" attrs))
         (continue)]
        [(and (start-tag _ #f name attrs) token)
         (yield token)
         (scan-match name)
         (kont)]
        [(and (start-tag _ #t _ attrs) token)
         (yield token)
         (kont)]
        [(? eof-object?)
         (handle-eof)]
        [(and (end-tag _ name) token) (=> continue)
         (handle-end-tag name token continue)]
        [token
         (handle-other-token token)]))

    (scan-prolog)
    ;; [WFC: Single root element]
    (scan-body void
               (lambda ()
                 (wfc-error "document production violation" eof))
               (lambda (name token continue) (continue))
               (lambda (token)
                 (error 'wfc "expected start tag, got: ~a~%" token)))
    (scan-misc*)

    (match (next-token)
      [(? eof-object? token) (yield token)]
      [token (error 'wfc "expected eof got: ~s~%" token)])))

