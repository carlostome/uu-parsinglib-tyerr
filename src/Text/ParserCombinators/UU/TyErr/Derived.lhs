%if style == newcode
\begin{code}
{-# LANGUAGE  RankNTypes,
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleInstances,
              FlexibleContexts,
              UndecidableInstances,
              NoMonomorphismRestriction #-}

{-# LANGUAGE  DataKinds,
              TypeOperators,
              TypeFamilies,
              AllowAmbiguousTypes,
              PolyKinds,
              ConstraintKinds #-}
module Text.ParserCombinators.UU.TyErr.Derived where

import           Text.ParserCombinators.UU.Core    (Alternative (..), IsParser)
import qualified Text.ParserCombinators.UU.Derived as Derived

import           GHC.TypeErrors
import           GHC.TypeErrors.PP
import           GHC.TypeErrors.Utils
import           GHC.TypeLits
\end{code}
%endif

In the library there are some combinators whose type we can not exploit to
provide the user with a domain specific error. Among this we can encounter the
following examples.

\begin{code}
pReturn :: Applicative p => a -> p  a
pReturn  = Derived.pReturn

pFail :: Alternative  p => p  a
pFail    = Derived.pFail

pMaybe :: IsParser p => p a -> p (Maybe a)
pMaybe   = Derived.pMaybe
\end{code}

For these cases, we just use the original type as defined by uu-parsinglib.

As a warm up example were we can exploit the customization of type errors we
have the following combinator:

%if style /= newcode
\begin{code}
pEither :: IsParser p => p a -> p b -> p (Either a b)
\end{code}
%endif

In this case, we can use the fact that we know that the parser of the first
argument has to be the same as the second one and if this is not the case
we are able to customize the error message. We can encode this as:

\begin{code}
pEither :: CustomErrors
  ![  ![ p1 :/~: p  :=>: VCat  ![Text "The parsers of the arguments for the function" :<+>:
                                 Quote (Text "pEither") :<+>: Text "do not coincide:"
                               ^^,Indent 2 (Text "The first  parser is" :<+>: ShowType p1 )
                               ^^,Indent 2 (Text "The second parser is" :<+>: ShowType p  )   ]]
  ,   ![Check (IsParser p)]
   ]  =>  p1 a -> p b -> p (Either a b)
pEither = Derived.pEither
\end{code}

Another interesting case for error customization is when an arrow type is
expected as an argument and some other argument's type has a
relation with it. In the combinator with type:

%if style /= newcode
\begin{code}
(<$$>) ::  IsParser p => (a -> b -> c) -> p b -> p (a -> c)
\end{code}
%endif

First, we can see that the first argument is expected to be a function with at
least two arguments. Moreover, the second argument's type must coincide with the
underlying type of the parser |p|. The later error is dependent of the former
because if the first argument is not a function then there is nothing else to
check. Therefore, we will encode this with the following type signature.

\begin{code}
(<$$>) :: CustomErrors
  ![ ![ f :/~: (a -> b1-> c) :=>:
          VCat  ![ Text "Expected as 1st argument a function type of 2 arguments but got:"
                ^^,Indent 4 (ShowType f)]]
  ,  ![ b1 :/~: b  :=>:
          VCat  ![Text "The underlying type of the parser" :<+>: Quote (ShowType p)
                ^^,Indent 4 (ShowType b)
                ^^,Text "and the type of the 2nd argument of the function:"
                ^^,Indent 4 (ShowType f)
                ^^,Text "have to agree." ]]
   , ![ Check (IsParser p) ]
   ] => f -> p b -> p (a -> c)
(<$$>) = (Derived.<$$>)
\end{code}

It is interesting to note that through the customization of errors we will find
ourselves many times with having multiple arguments that require the same
parser. Therefore, by using some type level machinery we abstracted over this
case providing the type level function |DifferentParsers (f :: Symbol) (p ::
[(k,Nat)])|. This function defined in  expectes the name
of the function we are customizing along a type level list of parsers numbered
with the argument were they appear.

With this tools, we can encode a custom error for the following combinator:

%if style /= newcode
\begin{code}
(<??>) :: IsParser p => p a -> p (a -> a) -> p a
\end{code}
%endif

As:

\begin{code}
(<??>) :: CustomErrors
  ![ ![ (p1 :/~: p) :=>: DifferentParsers "<??>" ![ !(p1,1) , !(p,2)]]
   , ![ f :/~: (a -> a)  :=>:
          VCat  ![ Text "Expected the underlying type of the 2nd parser to be a function type"
                   :<+>: Text "of 1 argument, but got:"
                ^^,Indent 4 (ShowType f)]]
   , ![ a :/~: a1        :=>:
          VCat  ![Text "The underlying type of the 1st parser" :<+>: Quote (ShowType p)
                ^^,Indent 4 (ShowType a1)
                ^^,Text "and the type of source and target of function inside the 2nd parser:"
                ^^,Indent 4 (ShowType f)
                ^^,Text "have to agree." ]]
   , ![ Check (IsParser p) ]
   ] => p1 a1 -> p f -> p a1
(<??>)         = (Derived.<??>)
\end{code}

We should also note that the case where a parser is wrapping an arrow type.
Therefore, we can also abstract over this fact with the type level function
|FunctionTypeParser (arg :: Nat) (f :: Symbol) (n :: Nat) :: ErrorMessage| defined in
\todo{add reference}.

The follwowing two combinators, show a similarity between their types. We can
think that besides both parsers being the same parsers and the underlying types
being functions of one argument, if the combination of source/target type of the
functions does not match maybe the user pretended to use the other one.

%if style /= newcode
\begin{code}
(<.>)  :: IsParser p => p (b -> c) -> p (a -> b) -> p (a -> c)
(<..>) :: IsParser p => p (a -> b) -> p (b -> c) -> p (a -> c)
\end{code}
%endif

\begin{code}
(<.>) :: CustomErrors
  ![ ![ p :/~: p2 :=>: DifferentParsers "<.>" ![ !(p, 1), !(p2 , 2) ]
      , f1 :/~: (b1 -> c)  :=>: FunctionTypeParser 1 f1 1
      , f2 :/~: (a -> b2)  :=>: FunctionTypeParser 2 f2 1]
   ,![ b1 :/~: b2          :=>:
          VCat  ![Text "The target type of the 2nd function:"
                ^^,Indent 4 (ShowType b2)
                ^^,Text "and the source type of the first one:"
                ^^,Indent 4 (ShowType b1)
                ^^,Text "should match."
                ^^,Text "Maybe you wanted to use (<.>) instead?"]]

   , ![ Check (IsParser p) ]
   ] => p f1 -> p2 f2 -> p (a -> c)
(<.>) = (Derived.<.>)

(<..>) :: CustomErrors
  ![ ![ p :/~: p2 :=>: DifferentParsers "<..>" ![ !(p, 1), !(p2 , 2) ]
      , f2 :/~: (b1 -> c)  :=>: FunctionTypeParser 1 f1 1
      , f1 :/~: (a -> b2)  :=>: FunctionTypeParser 2 f2 1]
   ,![ b1 :/~: b2          :=>:
          VCat  ![Text "The target type of the 2nd function:"
                ^^,Indent 4 (ShowType b2)
                ^^,Text "and the source type of the first one:"
                ^^,Indent 4 (ShowType b1)
                ^^,Text "should match."
                ^^,Text "Maybe you wanted to use (<..>) instead?"]]

   , ![ Check (IsParser p) ]
   ] => p f1 -> p2 f2 -> p (a -> c)
(<..>) = (Derived.<..>)
\end{code}

% Boring combinators
%if style == newcode
\begin{code}
infixl 4  <??>

pMany :: IsParser p => p a -> p [a]
pMany = Derived.pMany

pSome :: (IsParser f) => f a -> f [a]
pSome = Derived.pSome
\end{code}
%endif

An interesting case of this pattern occurs when the numbers of parsers involved
in the type of a combinator is greater than two. For example,

%if style /= newcode
\begin{code}
pPacked :: IsParser p => p b1 -> p b2 -> p a -> p a
\end{code}
%endif

\begin{code}
pPacked :: CustomErrors
  ![ ![ p  :/~: p1   :=>: DifferentParsers "pPacked" ![ !(p, 1), !(p1 , 2), !(p2, 3) ]
      , p1 :/~: p2   :=>: DifferentParsers "pPacked" ![ !(p, 1), !(p1 , 2), !(p2, 3) ]]
   , ![ Check (IsParser p) ]
   ] => p b1 -> p1 b2 -> p2 a -> p a
pPacked = Derived.pPacked
\end{code}

\begin{code}
pFoldr    :: IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr = Derived.pFoldr

pFoldr_ng ::  IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr_ng = Derived.pFoldr_ng


pFoldr1    :: IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1 = Derived.pFoldr1

pFoldr1_ng ::  IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1_ng = Derived.pFoldr1_ng
\end{code}

%if style == newcode
\begin{code}
pList    :: IsParser p => p a -> p [a]
pList     = Derived.pList

pList_ng :: IsParser p => p a -> p [a]
pList_ng  = Derived.pList_ng

pList1    :: IsParser p =>  p a -> p [a]
pList1     = Derived.pList1
pList1_ng :: IsParser p => p a -> p [a]
pList1_ng  = Derived.pList1_ng
\end{code}
%endif

\begin{code}
pFoldrSep    ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep = Derived.pFoldrSep

pFoldrSep_ng ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep_ng = Derived.pFoldrSep_ng

pFoldr1Sep    ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep = Derived.pFoldr1Sep

pFoldr1Sep_ng ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep_ng = Derived.pFoldr1Sep_ng

type PListSep (name :: Symbol) = forall p p1 a a1.
  CustomErrors
    ![ ![ p  :/~: p1   :=>: DifferentParsers name ![ !(p, 1), !(p1 , 2)]]
     , ![ Check (IsParser p) ]
     ] => p a1 -> p1 a -> p [a]

pListSep     :: PListSep "pListSep"
pListSep     = Derived.pListSep

pListSep_ng  :: PListSep "pListSep_ng"
pListSep_ng  = Derived.pListSep_ng

pList1Sep    :: PListSep "pList1Sep"
pList1Sep    = Derived.pList1Sep

pList1Sep_ng :: PListSep "pList1Sep_ng"
pList1Sep_ng = Derived.pList1Sep_ng

type PChain (name :: Symbol) = forall p p1 fc c1 c.
  CustomErrors
    ![ ![ p  :/~: p1  :=>: DifferentParsers name ![ !(p, 1), !(p1 , 2)]
        , fc :/~: (c1 -> c1 -> c1) :=>: FunctionTypeParser 1 fc 2 ]
     , ![ c1 :/~: c :=>: Text "Dummy" ]
     , ![ Check (IsParser p) ]
     ] => p fc -> p1 c -> p c

pChainr :: PChain "pChainr"
pChainr = Derived.pChainr

pChainr_ng :: PChain "pChainr_ng"
pChainr_ng = Derived.pChainr_ng

pChainl :: PChain "pChainl"
pChainl = Derived.pChainl

pChainl_ng :: PChain "pChainl_ng"
pChainl_ng = Derived.pChainl_ng
\end{code}

There are some combinators that share a common pattern for repeatedly applying a 
given parser a fixed number of times. These are,

%if style /= newcode
\begin{code}
pExact   :: (IsParser f) => Int -> f a -> f [a]
pAtLeast :: (IsParser f) => Int -> f a -> f [a]
pAtMost  :: (IsParser f) => Int -> f a -> f [a]
\end{code}
%endif

The ideal custom error message for this situation would be to firs check
whether the first argument is an |Int|. If this is not the case, maybe is of
type parser |p a|.  The same could be done for the second argument but the
other way around. However, in its current state we cannot conditionally check
this situation. Therefore we will conform ourselves with a suggestion to the
user that it may be the case that the arguments are swapped.

In order to encode all the three cases together we will make use of some type
level machinery.

\begin{code}
type Repeating (name :: Symbol) = forall int pa p a.
  CustomErrors
    ![ ![ int :/~: Int  :=?>:
            !( ![int ~ pa :=!>: Text "The 2nd argument is an Int, Maybe the arguments are swapped?"]
             ,  Text "The 2nd argument to" :<+>: Quote (ShowType name) :<+>:
                Text "has to be the parser to apply.")
        , pa :/~: p a   :=?>:
            !( ![pa ~ Int :=!>: Text "The 2nd argument is an Int, Maybe the arguments are swapped?"]
             ,  Text "The 2nd argument to" :<+>: Quote (ShowType name) :<+>:
                Text "has to be the parser to apply.")]
      , ![ Check (IsParser p) ]
      ] => int -> pa -> p [a]
\end{code}

And now we simply need to write the type signatures using |Repeating| with the
appropiate type level |String| for the name of the function. Maybe this could
be done more automatically by means of Template Haskell.

\begin{code}
pExact   :: Repeating "pExact"
pExact   = Derived.pExact

pAtLeast :: Repeating "pAtLeast"
pAtLeast = Derived.pAtLeast

pAtMost  :: Repeating "pAtMost"
pAtMost  = Derived.pAtMost
\end{code}

\todo{if not solved talk about the error message}

We can even scalate the pattern of advising for a wrong argument order situation
to functions that receive more than one |Int| as paramter. For example the following
combinator,
%if style /= newcode
\begin{code}
pBetween :: (IsParser f) => Int -> Int -> f a -> f [a]
\end{code}
%endif

gets the following customized error message,

\begin{code}
pBetween ::
  CustomErrors
    ![ ![ int1 :/~: Int  :=>:
            VCat  ![Text "The 1st argument to pBetween must be the minimum number of elements (Int) to be recognised."
                  ^^,Indent 4 (Text "The actual type is:" :<+>: ShowType int1)
                  ^^,Text "Maybe the order of arguments is wrong?" ]
        ,  int2 :/~: Int  :=>:
            VCat  ![Text "The 2st argument to pBetween must be the maximum number of elements (Int) to be recognised."
                  ^^,Indent 4 (Text "The actual type is:" :<+>: ShowType int2)
                  ^^,Text "Maybe the order of arguments is wrong?" ]
        , fa :/~: f a   :=?>:
            !( ![fa ~ Int :=!>: Text "The 3nd argument is an Int, Maybe the arguments are in the wrong order?"]
             ,  Text "The 3rd argument to pBetween has to be the parser to apply.")]
      , ![ Check (IsParser f) ]
      ] => int1 -> int2 -> fa -> f [a]
pBetween = Derived.pBetween
\end{code}

% Uninteresting cases.

%if style == newcode
\begin{code}
pCount :: (IsParser p, Num b) => p a -> p b
pCount = Derived.pCount

pAny :: IsParser p => (a -> p a1) -> [a] -> p a1
pAny  = Derived.pAny
\end{code}
%endif
