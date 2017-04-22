[![Build Status](https://travis-ci.org/philnguyen/z3-rkt.svg?branch=master)](https://travis-ci.org/philnguyen/bnf) bnf
=========================================

Concise Typed Racket syntax for declaring data in BNF.

### Install

```
raco pkg install bnf
```

### Examples

Defining syntax of λ-calculus terms `e` with based type `Number` and
meta-function `fv` computing its free variables:
```racket
(e . ::= . (Lam x e)
           (App e e)
           x
           Number)
(x . ::= . Symbol)

(: fv : e → (Setof x))
(define fv
  (match-lambda
    [(Lam x e*) (set-remove (fv e*) x)]
    [(App e₁ e₂) (set-union (fv e₁) (fv e₂))]
    [(? x? x) {set x}]
    [(? number?) {set}]))

(fv (App (Lam 'x (App 'x 'x)) (Lam 'y (App 'y 'y)))) ; ==> (set)
(fv (Lam 'x 'y)) ; ==> (set 'y)
```

You can specify existing type with identifiers or quoted datum, or `[#:old Type]`.
In the example below, the macro would generate new struct `Pairof` without
the `#:old` declaration.
```racket
(Tree . ::= . 'nil [#:old (Pairof Tree Tree)])

(: height : Tree → Natural)
(define height
  (match-lambda
    ['nil 0]
    [(cons l r) (+ 1 (max (height l) (height r)))]))

(height (cons (cons 'nil 'nil) 'nil)) ; ==> 2
```
