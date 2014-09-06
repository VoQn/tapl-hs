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
> chapter4
tapl>
```

### for Developers
```shell
> cabal sandbox init
> cabal install --only-dependencies
> cabal configure --enable-tests --enable-library-coverage
> cabal build
> run-cabal-test --show-details=always
> cabal run chapter4
```

## Sarith (by TaPL-Chapter-4)
**Sarith** is programming language according to TaPL Chapter4

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
