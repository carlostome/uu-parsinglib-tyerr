%if style == newcode
\begin{code}
{-# LANGUAGE  RankNTypes,
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleInstances,
              FlexibleContexts,
              UndecidableInstances,
              NoMonomorphismRestriction,
              DataKinds,
              TypeOperators,
              TypeFamilies,
              AllowAmbiguousTypes,
              PolyKinds,
              ConstraintKinds #-}


module Text.ParserCombinators.UU.TyErr.Derived where

import qualified Text.ParserCombinators.UU.Derived as Derived
import qualified Text.ParserCombinators.UU.Core    as Core
import Text.ParserCombinators.UU.Core (IsParser, Alternative(..))

import GHC.TypeErrors
import GHC.TypeLits
import GHC.TypeErrors.Utils
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

> pEither :: IsParser p => p a -> p b -> p (Either a b)

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

% -- | `<$$>` is the version of `<$>` which flips the function argument
> (<$$>) ::  IsParser p => (a -> b -> c) -> p b -> p (a -> c)

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
|FunctionType (arg :: Nat) (f :: Symbol) (n :: Nat) :: ErrorMessage| defined in
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
      , f1 :/~: (b1 -> c)  :=>: FunctionType 1 f1 1
      , f2 :/~: (a -> b2)  :=>: FunctionType 2 f2 1]
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
      , f2 :/~: (b1 -> c)  :=>: FunctionType 1 f1 1
      , f1 :/~: (a -> b2)  :=>: FunctionType 2 f2 1]
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

%
% -- -- * Iterating combinators, all in a greedy (default) and a non-greedy (ending with @_ng@) variant
%
% -- -- ** Recognising  list like structures
% -- pFoldr    :: IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
% -- pFoldr         alg@(op,e)     p =  must_be_non_empty "pFoldr" p pfm
% --                                    where pfm = (op <$> p <*> pfm) `opt` e
%
% -- pFoldr_ng ::  IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
% -- pFoldr_ng      alg@(op,e)     p =  must_be_non_empty "pFoldr_ng" p pfm
% --                                    where pfm = (op <$> p <*> pfm)  <|> pure e
%
%
% -- pFoldr1    :: IsParser p => (v -> b -> b, b) -> p v -> p b
% -- pFoldr1        alg@(op,e)     p =  must_be_non_empty "pFoldr1"    p (op <$> p <*> pFoldr     alg p)
%
% -- pFoldr1_ng ::  IsParser p => (v -> b -> b, b) -> p v -> p b
% -- pFoldr1_ng     alg@(op,e)     p =  must_be_non_empty "pFoldr1_ng" p (op <$> p <*> pFoldr_ng  alg p)
%
%
% -- list_alg :: (a -> [a] -> [a], [a1])
% -- list_alg = ((:), [])
%
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

%
% -- -- * Recognising list structures with separators
%
% -- pFoldrSep    ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
% -- pFoldrSep      alg@(op,e) sep p =  must_be_non_empties "pFoldrSep" sep   p
% --                                    (op <$> p <*> pFoldr    alg sepp `opt` e)
% --                                    where sepp = sep *> p
% -- pFoldrSep_ng ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
% -- pFoldrSep_ng   alg@(op,e) sep p =  must_be_non_empties "pFoldrSep" sep   p
% --                                    (op <$> p <*> pFoldr_ng alg sepp <|>  pure e)
% --                                    where sepp = sep *> p
%
% -- pFoldr1Sep    ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
% -- pFoldr1Sep     alg@(op,e) sep p =  must_be_non_empties "pFoldr1Sep"    sep   p pfm
% --                                    where pfm = op <$> p <*> pFoldr    alg (sep *> p)
% -- pFoldr1Sep_ng ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
% -- pFoldr1Sep_ng  alg@(op,e) sep p =  must_be_non_empties "pFoldr1Sep_ng" sep   p pfm
% --                                    where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)
%
% -- pListSep    :: IsParser p => p a1 -> p a -> p [a]
% -- pListSep      sep p = must_be_non_empties "pListSep"    sep   p (pFoldrSep     list_alg sep p)
% -- pListSep_ng :: IsParser p => p a1 -> p a -> p [a]
% -- pListSep_ng   sep p = must_be_non_empties "pListSep_ng" sep   p pFoldrSep_ng  list_alg sep p
%
% -- pList1Sep    :: IsParser p => p a1 -> p a -> p [a]
% -- pList1Sep     s p =  must_be_non_empties "pListSep"    s   p (pFoldr1Sep    list_alg s p)
% -- pList1Sep_ng :: IsParser p => p a1 -> p a -> p [a]
% -- pList1Sep_ng  s p =  must_be_non_empties "pListSep_ng" s   p (pFoldr1Sep_ng list_alg s p)
%
% -- -- * Combinators for chained structures
% -- -- ** Treating the operator as right associative
% -- pChainr    :: IsParser p => p (c -> c -> c) -> p c -> p c
% -- pChainr    op x    =   must_be_non_empties "pChainr"    op   x r where r = x <??> (flip <$> op <*> r)
% -- pChainr_ng :: IsParser p => p (c -> c -> c) -> p c -> p c
% -- pChainr_ng op x    =   must_be_non_empties "pChainr_ng" op   x r where r = x <**> ((flip <$> op <*> r)  <|> pure id)
%
% -- -- ** Treating the operator as left associative
% -- pChainl    :: IsParser p => p (c -> c -> c) -> p c -> p c
% -- pChainl   op x    =  must_be_non_empties "pChainl"    op   x (f <$> x <*> pList (flip <$> op <*> x))
% --                     where  f x [] = x
% --                            f x (func:rest) = f (func x) rest
% -- pChainl_ng :: IsParser p => p (c -> c -> c) -> p c -> p c
% -- pChainl_ng op x    = must_be_non_empties "pChainl_ng" op   x (f <$> x <*> pList_ng (flip <$> op <*> x))
% --                      where f x [] = x
% --                            f x (func:rest) = f (func x) rest
%
% -- -- * Repeating parsers
%

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
type Repeating (name :: Symbol) = forall int fa f a.
  CustomErrors
    ![ ![ int  :/~: Int   :=>:
            VCat  ![Text "The 1st argument to" :<+>: Quote (ShowType name) :<+>:
                      Text "must be the number of elements to be recognised."
                  ^^,Text "Maybe the arguments are swapped?" ]
        , fa   :/~: f a   :=>:
            VCat  ![Text "The 2nd argument to" :<+>: Quote (ShowType name) :<+>:
                       Text "is the parser to apply."
                  ^^,Text "Maybe the arguments are swapped?" ]]
      , ![ Check (IsParser f) ]
      ] => int -> fa -> f [a]
\end{code}

And now we simply need to write the type signatures using |Repeating| with the
appropiate type level |String| for the name of the function. Maybe this could
be done more automatically by means of Template Haskell.

\begin{code}
pExact :: Repeating "pExact"
pExact = Derived.pExact

pAtLeast :: Repeating "pAtLeast"
pAtLeast = Derived.pAtLeast

pAtMost :: Repeating "pAtMost"
pAtMost = Derived.pAtMost
\end{code}

%
% -- pBetween :: (IsParser f) => Int -> Int -> f a -> f [a]
% -- pBetween m n p |  n < 0 || m <0 =  error "negative arguments to pBwteeen"
% --                |  m > n         =  empty
% --                |  otherwise     =  (++) <$> pExact m p <*> pAtMost (n-m) p
%

% Boring cases
%if style == newcode
\begin{code}
pCount :: (IsParser p, Num b) => p a -> p b
pCount = Derived.pCount

pAny :: IsParser p => (a -> p a1) -> [a] -> p a1
pAny  = Derived.pAny
\end{code}
%endif

% --------------------------------------------------------------------------------
%   -- Custom Type Errors tailored for Parser
%

\label{differentparsers}
\begin{code}
type family DifferentParsers (f :: Symbol) (p :: [(k,Nat)]) where
  DifferentParsers f p =
    Text "The parsers of the arguments for" :<+>: Text f :<+>: Text "do not coincide:" :$$:
      Indent 4 (VCat (Map MakeParserArgSym p))

type family MakeParserArg p where
  MakeParserArg !(p,n) = Text "The parser of the #" :<>: ShowType n :<+>:
                        Text "argument is" :<+>: ShowType p :<>: Dot

data MakeParserArgSym :: ((k , Nat) ~> ErrorMessage) -> *

type instance Apply MakeParserArgSym x = MakeParserArg x
\end{code}
