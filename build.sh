#!/bin/bash

cabal sandbox init
cabal install   --only-dependencies --with-compiler=/home/carlos/Dev/Haskell/ghc/inplace/bin/ghc-stage2 --package-db=home/carlos/Dev/Haskell/ghc/inplace/lib/package.conf.d --allow-newer=template-haskell
cabal configure --with-compiler=/home/carlos/Dev/Haskell/ghc/inplace/bin/ghc-stage2 --package-db=home/carlos/Dev/Haskell/ghc/inplace/lib/package.conf.d --allow-newer=template-haskell
cabal build

