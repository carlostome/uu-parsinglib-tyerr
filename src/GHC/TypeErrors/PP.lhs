%if style == newcode
\begin{code}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
module GHC.TypeErrors.PP where

import           GHC.TypeErrors
import           GHC.TypeLits

\end{code}
%endif

In this module a basic set of combinators for type level pretty printing
of error messages are defined. This module is library independent and maybe
it can be completed and made into its own library.

As an aside, the optimal option would be that GHC supports all this combinators
by default as they are a type level reflection of the custom Pretty printing
that GHC uses internally for displaying error messages to the user.

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

data (~>) :: * -> * -> *

type family Apply (f :: (k1 ~> k2) -> * ) (x :: k1) :: k2

type family Map (f :: (k1 ~> k2) -> * ) (xs :: [k1]) :: [k2] where
  Map f nil = nil
  Map f (x !: xs) = Apply f x !: (Map f xs)
\end{code}
