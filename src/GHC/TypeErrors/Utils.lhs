%if style == newcode
\begin{code}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
module GHC.TypeErrors.Utils where

import           GHC.TypeErrors
import           GHC.TypeLits
import           GHC.TypeErrors.PP

\end{code}
%endif

This module defines domain specific combinators for type error messages
for the library.

\label{functiontype}
\begin{code}
type FunctionType (arg :: Nat) (f :: *) (n :: Nat) =
  VCat  ![Text "Expected as #" :<>: ShowType arg :<+>:
        ^^ Text "argument a function type of" :<+>: ShowType n :<+>:
        ^^ Text "arguments but got" :<>: Colon
        ^^,Indent 4 (ShowType f) :<>: Dot ]

type FunctionTypeParser (arg :: Nat) (f :: *) (n :: Nat) =
  VCat  ![Text "Expected as #" :<>: ShowType arg :<+>:
        ^^ Text "argument a parser with an underlying function type of" :<+>: ShowType n :<+>:
        ^^ Text "arguments but got" :<>: Colon
        ^^,Indent 4 (ShowType f) :<>: Dot ]

type family DifferentParsers (f :: Symbol) (p :: [(k,Nat)]) where
  DifferentParsers f p =
    Text "The parsers of the arguments for" :<+>: Text f :<+>: Text "do not coincide:" :$$:
      Indent 4 (VCat (Map MakeParserArgSym p))

type family MakeParserArg p where
  MakeParserArg !(p,n) = Text "The parser of the #" :<>: ShowType n :<+>:
                         Text "argument is" :<+>: ShowType p :<>: Dot

data MakeParserArgSym :: ((k , Nat) ~> ErrorMessage) -> *

type instance Apply MakeParserArgSym x = MakeParserArg x

type ExpectedErrorMessage (name :: Symbol) (argn :: Nat) (descr :: Symbol) t =
  VCat  ![Text "The #" :<>: ShowType argn :<+>: Text "argument to" :<+>: Quote (Text name)
          :<+>: Text "is expected to be" :<+>: Text descr :<>: Text ", but its type is" :<>: Colon
        ^^,Empty
        ^^,Indent 2 (ShowType t) ]

\end{code}
