# uu-parsinglib-tyerr

Wrapper arround
[uu-parsinglib](https://hackage.haskell.org/package/uu-parsinglib) with
customized type error messages.

## Build as a Haskell library

In order to use it as a Haskell library is necessary to have installed the
patch to GHC that makes the magic of custom error messages.

It is neccesary to have [lhs2TeX](https://www.andres-loeh.de/lhs2tex/) somewhere
in the path as the `Setup.hs` file has been customized to preprocces the
modules.

Supposing that GHC is avaliable in `/path/to/ghc` (absolute path) the steps to build the library are the
following:

```
cabal sandbox init
cabal install   --only-dependencies --with-compiler=/path/to/compiler/inplace/bin/ghc-stage2 --package-db=home/carlos/Dev/Haskell/ghc/inplace/lib/package.conf.d --allow-newer=template-haskell
cabal configure --with-compiler=/path/to/compiler/inplace/bin/ghc-stage2 --package-db=path/to/compiler/inplace/lib/package.conf.d --allow-newer=template-haskell
cabal build
```

## Build as a PDF

This library is engineered so it can be built as one big documentation file with
all the modules and the latex comments they contain.

This needs [lhs2TeX](https://www.andres-loeh.de/lhs2tex/) and
[latexmk](https://www.ctan.org/pkg/latexmk/) to be built.

If those are avaliable then just type:

```
make doc
```
