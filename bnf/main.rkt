#lang typed/racket/base

(provide ::=
         (rename-out [::= ⩴])
         (rename-out [define-type ≜])
         define-substructs)

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     racket/pretty)
         typed-struct-props
         syntax/parse/define)

(begin-for-syntax
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
    (pattern (s:id f ...)
             #:attr name #'s
             #:attr def
             (syntax-parse #'(f ...)
               [fs:flds
                (with-syntax ([(fld ...) (attribute fs.gen)])
                  #`(struct/props s (fld ...) #:transparent
                                  #:property prop:custom-write
                                  (λ (this o _)
                                    (fprintf o "(")
                                    (write 's o)
                                    #,@(map (syntax-parser
                                              [(f _ _)
                                               (with-syntax ([s-f (format-id #'f "~a-~a" #'s #'f)])
                                                 #'(begin
                                                     (fprintf o " ")
                                                     (print (s-f this) o)))])
                                            (syntax->list #'(fld ...)))
                                    (fprintf o ")"))))]))
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
  [(t:id . _ . (k:id f₁ f₂) #:ad-hoc)
   #:when (free-identifier=? #'t #'k)
   (syntax-parse #'(f₁ f₂)
     [f:flds
      (syntax-parse (attribute f.gen)
        [([x₁ _ T₁] [x₂ _ T₂])
         (with-syntax ([t-x₁ (format-id #'x₁ "~a-~a" #'t #'x₁)]
                       [t-x₂ (format-id #'x₂ "~a-~a" #'t #'x₂)]
                       [mk-t (format-id #'t  "mk-~a" #'t)])
           #'(begin
               (define-type t (Pairof T₁ T₂))
               (define mk-t (ann cons (T₁ T₂ → t)))
               (define t-x₁ (ann car (t → T₁)))
               (define t-x₂ (ann cdr (t → T₂)))))])])]
  [(t:id . _ . #:TBD)
   #'(struct t () #:transparent)]
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
          #'(begin
            (struct K T (f* ...) #:transparent) ...))])]))

;; Define type `t` along with predicate `t?`
(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(_ τ e) (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
               #'(begin (define-type τ e)
                        (define-predicate τ? τ)))]))
