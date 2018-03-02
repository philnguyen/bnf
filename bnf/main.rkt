#lang typed/racket/base

(provide ::=
         (rename-out [::= ⩴]))

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     racket/pretty)
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
             (with-syntax ([(fld ...) (parse-fields #'(f ...))])
               #'(struct s (fld ...) #:transparent)))
    ;; prevent generating new types, e.g. (Pairof _ _), (Listof _), etc.
    (pattern [#:reuse t]
             #:attr name #'t
             #:attr def #f))

  (define (parse-fields stx)
    (for/list ([(fᵢ i) (in-indexed (syntax->list stx))])
      (syntax-parse fᵢ
        [(_:id (~literal :) _) fᵢ]
        [tᵢ (with-syntax ([xᵢ (format-id #'t "_~a" i)])
              #'(xᵢ : tᵢ))])))
  )

(define-syntax-parser ::=
  [(t:id . _ . (~and RHS:rhs (k:id f ...)))
   #:when (free-identifier=? #'t #'k)
   #'RHS.def]
  [(LHS:id . _ . RHS:rhs ...)
   (with-syntax ([(def-struct ...)
                  (filter-map
                   (syntax-parser [r:rhs (attribute r.def)])
                   (syntax->list #'(RHS ...)))])
     #'(begin
         def-struct ...
         (define-type/pred LHS (U RHS.name ...))))])

;; Define type `t` along with predicate `t?`
(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(_ τ e) (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
               #'(begin (define-type τ e)
                        (define-predicate τ? τ)))]))
