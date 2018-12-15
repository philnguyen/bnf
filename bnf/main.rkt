#lang typed/racket/base

(provide ::=
         ≜
         ≗
         (rename-out [::= ⩴])
         define-substructs)

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     racket/pretty)
         (for-meta 1 ; I'm confused. Thought `2` but `1` worked.
                   racket/base
                   syntax/transformer)
         racket/match
         racket/string
         racket/pretty
         typed/racket/unsafe
         typed-struct-props
         syntax/parse/define)

(unsafe-require/typed racket/struct
  [make-constructor-style-printer
   (∀ (A) ((A → (U Symbol String))
           (A → (Listof Any))
           → A Output-Port (U #t #f 0 1) → Void))]
  [struct->list (∀ (A) (A → (Listof Any)))])

(define-syntax-rule (struct* K args ...)
  (struct/props K args ...
                #:transparent
                #:property prop:custom-write
                ((inst make-constructor-style-printer K)
                 (λ (_) 'K)
                 struct->list)))

(begin-for-syntax 
  (define (gen-ad-hoc-pair-defns t k f₁ f₂)
    (syntax-parse #`(#,f₁ #,f₂)
      [f:flds
       (syntax-parse (attribute f.gen)
         [([x₁ _ T₁] [x₂ _ T₂])
          (with-syntax ([t-x₁ (format-id #'x₁ "~a-~a" t #'x₁)]
                        [t-x₂ (format-id #'x₂ "~a-~a" t #'x₂)]
                        [mk-t (if (free-identifier=? t k)
                                  (format-id t  "mk-~a" t)
                                  k)])
            #`(begin
                (define-type #,t (Pairof T₁ T₂))
                (define-match-expander mk-t
                  (syntax-rules ()
                    [(_ x y) (cons x y)])
                  (make-variable-like-transformer #'(ann cons (T₁ T₂ → #,t))))
                (define t-x₁ (ann car (#,t → T₁)))
                (define t-x₂ (ann cdr (#,t → T₂)))))])]))
  
  (define-syntax-class rhs
    #:description "right-hand side of BNF rule"
    #:attributes (name def)
    (pattern x:id
             #:attr name #'x
             #:attr def #f)
    (pattern ((~literal quote) v)
             #:attr name #''v
             #:attr def #f)
    (pattern (~or v:boolean v:str v:number v:char)
             #:attr name #''v
             #:attr def #f)
    (pattern (t:id f₁ f₂ #:ad-hoc)
             #:attr name #'t
             #:attr def (gen-ad-hoc-pair-defns #'t #'t #'f₁ #'f₂))
    (pattern (s:id f ...)
             #:attr name #'s
             #:attr def
             (let-values ([(fs mut?)
                           (syntax-parse #'(f ...)
                             [(f ... #:mutable) (values #'(f ...) #t)]
                             [_                 (values #'(f ...) #f)])])
               (syntax-parse fs
                 [fs:flds
                  (with-syntax ([(fld ...) (attribute fs.gen)])
                    (if mut?
                        #'(struct* s (fld ...) #:mutable)
                        #'(struct* s (fld ...))))])))
    ;; prevent generating new types, e.g. (Pairof _ _), (Listof _), etc.
    (pattern [#:reuse t]
             #:attr name #'t
             #:attr def #f))
  (define-syntax-class flds
    #:description "field list description"
    #:attributes (gen)
    (pattern (f ...)
             #:attr gen
             (for/list ([(f i) (in-indexed (syntax->list #'(f ...)))])
               (syntax-parse f
                 [(_:id (~literal :) _) f]
                 [t (with-syntax ([x (format-id #'t "_~a" i)])
                      #'(x : t))]))))
  )

(define-syntax-parser ::=
  [(t:id . _ . (~and RHS:rhs (k:id f ...)))
   #:when (free-identifier=? #'t #'k)
   #'RHS.def]
  [(t:id . _ . #:TBD) #'(struct* t ())]
  [(LHS:id . _ . RHS:rhs ...)
   (with-syntax ([(def-struct ...)
                  (filter-map
                   (syntax-parser [r:rhs (attribute r.def)])
                   (syntax->list #'(RHS ...)))])
     #'(begin
         def-struct ...
         (define-type/pred LHS (U RHS.name ...))))])

(define-syntax define-substructs
  (syntax-parser
    [(_ T:id (K:id f ...) ...)
     (syntax-parse #'((f ...) ...)
       [(fs:flds ...)
        (with-syntax ([((f* ...) ...) (attribute fs.gen)])
          #'(begin (struct* K T (f* ...)) ...))])]))

;; Define type `t` along with predicate `t?`
(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(_ τ e) (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
               #'(begin (define-type τ e)
                        (define-predicate τ? τ)))]))

(define-syntax ≜
  (syntax-parser
    [(t:id . _ . (k:id l:expr r:expr) #:ad-hoc)
     (gen-ad-hoc-pair-defns #'t #'k #'l #'r)]
    [(lhs . _ . rhs) #'(define-type lhs rhs)]))

(define-simple-macro (lhs . ≗ . rhs) (define-new-subtype lhs rhs))
