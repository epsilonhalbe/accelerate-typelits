Accelerate TypeLits
===================

[![Build Status](https://travis-ci.org/epsilonhalbe/accelerate-typelits.svg?branch=master)](https://travis-ci.org/epsilonhalbe/accelerate-typelits)
Synopsis
--------

This library provides a high level interface to `accelerate` for matrix
computations.

Installation
------------

The simplest way to install this library is using `cabal` or `cabal-sandbox`

```
> cabal install accelerate-typelits
```

If you want to have the most recent version, the project is on github so you can
checkout the project.

```
> git clone https://github.com/epsilonhalbe/accelerate-typelits.git
> cd accelerate-typelits
> cabal install
```

---

```
> git clone https://github.com/epsilonhalbe/accelerate-typelits.git
> cd accelerate-typelits
> cabal sandbox init
> cabal install
```

There is also a `stack.yaml` file included, so one can also use [stack][1] in
order to compile this library.

```
> git clone https://github.com/epsilonhalbe/accelerate-typelits.git
> cd accelerate-typelits
> stack --stack-yaml stack-7.10.yaml build
```

---

The operators have been designed to give a visual hint of the respective
parameters.

- `#` for matrices
- `^` for vectors
- `.` for scalars

So for example `#*^` represents the multiplication of a matrix with a vector,
analogously `^*#` works the other way around. Other examples would be `#*#` for
matrix-matrix multiplication and `.*^` scalar multiplication of a vector.

Operator precedence is usually the same as the numeric equivalence.

Credits
-------

The matrix-vector and matrix-matrix products have been inspired by Henning
Thielemann's [`accelerate-arithmetic`][2] library

[1]: https://haskellstack.com
[2]: https://hackage.haskell.org/package/accelerate-arithmetic
