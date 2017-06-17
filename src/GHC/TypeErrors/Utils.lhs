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

\end{code}
%endif

We begin by defining a set of basic combinators to build up our prettyprinting
library.

\begin{code}
type Empty = Text ""
type Space = Text " "
type Colon = Text ":"
type Dot   = Text "."
type Comma = Text ","
type Quote n = Text "'" :<>: n :<>: Text "`"

type family (:<+>:) (a :: ErrorMessage) (b :: ErrorMessage) where
  a :<+>: b = a :<>: Space :<>: b

infixl 6 :<+>:
type family VCat a where
  VCat nil       = Empty
  VCat (x !: xs) = x :$$: VCat xs

type family HCat a where
  HCat nil       = Empty
  HCat (x !: xs) = x :<>: HCat xs

type family HSep a where
  HSep nil       = Empty
  HSep (x !: xs) = x :<+>: HSep xs

type family Indent (n :: Nat) (e :: ErrorMessage) where
  Indent 0 x = x
  Indent n x = Empty :<+>: Indent (n - 1) x
\end{code}


\label{functiontype}
\begin{code}
type FunctionType (arg :: Nat) (f :: Symbol) (n :: Nat) =
  VCat ![ Text "Expected as #" :<>: ShowType arg :<+>:
          Text "argument a function type of" :<+>: ShowType n :<+>:
          Text "arguments but got" :<>: Colon
        , Indent 4 (ShowType f) :<>: Dot ]

data (~>) :: * -> * -> *

type family Apply (f :: (k1 ~> k2) -> * ) (x :: k1) :: k2

type family Map (f :: (k1 ~> k2) -> * ) (xs :: [k1]) :: [k2] where
  Map f nil = nil
  Map f (x !: xs) = Apply f x !: (Map f xs)

\end{code}
