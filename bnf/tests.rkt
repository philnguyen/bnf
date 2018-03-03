#lang typed/racket/base

(require racket/match
         racket/set
         typed/rackunit
         "main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; λ-calculus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e . ::= . ℤ x (Lam x e) (App e e))
(x . ::= . Symbol)
(ℤ . ::= . Integer)

(: fv : e → (Setof x))
(define fv
  (match-lambda
    [(Lam x e*) (set-remove (fv e*) x)]
    [(App e₁ e₂) (set-union (fv e₁) (fv e₂))]
    [(? x? x) {set x}]
    [(? ℤ?) {set}]))

(define tt (Lam 'x (Lam 'y 'x)))
(define ff (Lam 'x (Lam 'y 'y)))
(define Ω (App (Lam 'x (App 'x 'x)) (Lam 'y (App 'y 'y))))
(define open (Lam 'x 'y))

(check-equal? (fv tt) {set})
(check-equal? (fv ff) {set})
(check-equal? (fv Ω) {set})
(check-equal? (fv open) {set 'y})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Tree . ::= . 'nil [#:reuse (Pairof Tree Tree)])

(: height : Tree → Natural)
(define height
  (match-lambda
    ['nil 0]
    [(cons l r) (+ 1 (max (height l) (height r)))]))

(check-equal? (height (cons (cons 'nil 'nil) 'nil)) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Trivial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Person . ::= . (Person [name : String] [age : Natural]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Sub-structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-substructs Person
  [Dog [id : String]]
  [Cat String Number])
