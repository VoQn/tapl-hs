# tapl-hs [![Build Status](http://img.shields.io/travis/VoQn/tapl-hs.svg?style=flat-square)](https://travis-ci.org/VoQn/tapl-hs) [![Coverage Status](https://img.shields.io/coveralls/VoQn/tapl-hs.svg?style=flat-square)](https://coveralls.io/r/VoQn/tapl-hs?branch=master) [![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square)](https://tldrlegal.com/license/mit-license)

## TaPL Language Implement by Haskell

This is an exercise repository of Implement Programming language according to the book [_TaPL (Types and Programming Language)_](http://www.cis.upenn.edu/~bcpierce/tapl/)

## How to Build

### for Players
```shell
> cabal sandbox init
> cabal install --only-dependencies
> cabal configure
> cabal build
> cabal install
> tapl-hs
TaPL-hs REPL
========================================
0) Exit
1) Sarith (from Chapter 3-4)
2) Sulamb (from Chapter 5-7)
----------------------------------------
Select Mode (default: 2) >> # enter language index (quit for 0)
Sulamb>
```

### for Developers
```shell
> cabal sandbox init
> cabal install --only-dependencies
> cabal configure --enable-tests --enable-library-coverage
> cabal build
> run-cabal-test --show-details=always
> cabal run
```

## Sarith (by TaPL-Chapter.3-4)
**Sarith** is programming language according to TaPL Chapter.3-4

This language's syntax is like __scheme__
### Basic Values (true/false/0)
```scheme
true  ; boolean true
false ; boolean false
0     ; natural number zero
```

### (succ &lt;term&gt;) & (pred &lt;term&gt;)
```scheme
(succ 0) ; => 1
(pred 0) ; => 0
(succ (succ 0)) ; => 2
(pred (succ 0)) ; => 0
(succ (pred (succ 0))) ; => 1

(succ true) ; => TypeError
(pred true) ; => TypeError
```

### (zero? &lt;term&gt;)
```scheme
(zero? 0) ; => true
(zero? (succ 0)) ; => false

(zero? false) ; => TypeError
```

### (if &lt;term&gt; &lt;term&gt; &lt;term&gt;)
```scheme
(if true 0 (succ 0)) ; => 0
(if false 0 (succ 0)) ; => (succ 0)

(if 0 true false) ; => TypeError
```

## Sulamb (by TaPL-Chapter.5-7)
**Sulamb** is programming language according to TaPL Chapter.5-7

This language based on untype-lambda-calculus.
All functions are auto curried.

### Basics (id)
```scheme
(\ x x)     ; identity function

(\ x (\ y y)) ; meaning λx.λy.y
(\ (x y) y)   ; meaning λxy.y (the same as λx.λy.y)

(f x) ; apply function `f` with `x`
(\ (l m n) (l m n)) ; λlmn.l m n

;; Example
(id) ; => (\ x x)
(id (\ y y)) ; => (\ y y)
(id (id (\ w w))) ; => (\ w w)
```

### Bool (tru/fls/tst/and/or)
```scheme
(tru)       ; Church-true
(fls)       ; Church-false
(tst p t f) ; Church-if
(and a b)   ; Church-and
(or  a b)   ; Church-or

;; Example
(tru) ; => (\ t (\ f t))
(fls) ; => (\ t (\ f f))
(and tru tru) ; => tru
(and fls tru) ; => fls
(or fls fls)  ; => fls
(or fls tru)  ; => tru

(tst tru (\ v v) (\ w w)) ; => (\ v v)
(tst fls (\ v v) (\ w w)) ; => (\ w w)
```

### Number (zrp/one/scc/zro?)
```scheme
(zro)    ; Church-zero
(one)    ; Church-one
(scc x)  ; Church-succ
(zro? x) ; Church-zero?

;; Example
(zro? zro) ; => tru
(zro? one) ; => fls
(zro? (scc zro)) ; => fls
```

### Collection (pir/fst/snd)
```scheme
(pir v w) ; Church-pair constructor
(fst p)   ; Church-fst
(snd p)   ; Church-snd

;; Example
(fst (pir (\ v v) (\ w w))) ; => (\ v v)
(snd (pir (\ v v) (\ w w))) ; => (\ w w)
```
