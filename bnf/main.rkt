#lang typed/racket/base

(provide ::=
         (rename-out [::= ⩴]))

(require (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/parse
                     racket/pretty)
         syntax/parse/define)

(begin-for-syntax
  (define-syntax-class rhs
    #:description "right-hand side of BNF rule"
    (pattern _:id)
    (pattern ((~literal quote) _))
    (pattern _:boolean)
    (pattern _:str)
    (pattern _:number)
    (pattern _:char)
    (pattern [#:reuse _]) ; prevent generating new types, e.g. (Pairof _ _), (Listof _), etc.
    (pattern (s:id _ ...)))

  (define (in-syntax-list x) (in-list (syntax->list x)))

  (define (parse-fields stx)
    (for/list ([fᵢ (in-syntax-list stx)] [i (in-naturals)])
      (syntax-parse fᵢ
        [(_:id (~literal :) _) fᵢ]
        [tᵢ (define/with-syntax xᵢ (format-id #'t "_~a" i))
            #'(xᵢ : tᵢ)])))

  (define (gen-def-structs rhs)
    (for*/list ([rhs (in-syntax-list rhs)]
                [?def
                 (in-value
                  (syntax-parse rhs
                    #:literals (quote)
                    [(quote _) #f]
                    [(s:id f ...)
                     (define/with-syntax (fld ...) (parse-fields #'(f ...)))
                     #'(struct s (fld ...) #:transparent)]
                    [_ #f]))]
                #:when ?def)
      ?def))
  
  (define (extract-rhs-names rhs)
    (for/list ([rhs (in-syntax-list rhs)])
      (syntax-parse rhs
        #:literals (quote)
        [(quote v) #''v]
        [(s:id _ ...) #'s]
        [(#:reuse t) #'t]
        [t #'t]))))

(define-syntax-parser ::=
  [(t:id . _ . (k:id f ...))
   #:when (free-identifier=? #'t #'k)
   (define/with-syntax (fld ...) (parse-fields #'(f ...)))
   #'(struct k (fld ...) #:transparent)]
  [(LHS:id . _ . RHS:rhs ...)
   (define/with-syntax (def-struct ...) (gen-def-structs #'(RHS ...)))
   (define/with-syntax def-union
     (syntax-parse (extract-rhs-names #'(RHS ...))
       [(     ) #'(define-type/pred LHS Nothing  )]
       [(t    ) #'(define-type/pred LHS t        )]
       [(t ...) #'(define-type/pred LHS (U t ...))]))
   (define gen
     #'(begin
         def-struct ...
         def-union))
   ;(printf "~a~n" (pretty-write (syntax->datum gen)))
   gen])

;; Define type `t` along with predicate `t?`
(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(_ τ e) (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
               #'(begin (define-type τ e)
                        (define-predicate τ? τ)))]))
