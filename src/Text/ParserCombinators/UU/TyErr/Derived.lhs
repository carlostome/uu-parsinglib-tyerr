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
              PolyKinds #-}


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
  ![ ![ DifferentParsers "<??>" ![ !(p1,1) , !(p,2)]]
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

%if style /= newcode
\begin{code}
(<.>) :: IsParser p => p (b -> c) -> p (a -> b) -> p (a -> c)
\end{code}
%endif

\begin{code}
(<.>) :: CustomErrors
  ![ ![ DifferentParsers "<.>" ![ !(p, 1), !(p2 , 2) ]
      , f1 :/~: (b1 -> c)  :=>: FunctionType 1 f1 1
      , f2 :/~: (a -> b2)  :=>: FunctionType 2 f2 1]
   ,![ b1 :/~: b2          :=>:
          VCat  ![Text "The target type of the 2nd function:"
                ^^,Indent 4 (ShowType b2)
                ^^,Text "and the source type of the first one:"
                ^^,Indent 4 (ShowType b1)
                ^^,Text "should match." ]]

   , ![ Check (IsParser p) ]
   ] => p f1 -> p2 f2 -> p (a -> c)
(<.>) = (Derived.<.>)
\end{code}

%
% -- -- | `<..>` functional composition of two parsers with the arguments reversed
% -- --
% -- (<..>) :: IsParser p => p (a -> b) -> p (b -> c) -> p (a -> c)
% -- g <..> f = (.) <$> f <*> g
%
%
% -- infixl 4  <??>
%
% -- -- | `pMany` is equivalent to the `many` from "Control.Applicative". We want however all our parsers to start with a lower case @p@.
% pMany :: IsParser p => p a -> p [a]
% pMany = Derived.pMany
%
% -- -- | `pSome` is equivalent to the `some` from "Control.Applicative". We want however all our parsers to start with a lower case @p@.
% pSome :: (IsParser f) => f a -> f [a]
% pSome = Derived.pSome
%
%
% -- -- | @`pPacked`@ surrounds its third parser with the first and the second one, returning only the middle result
% -- pPacked :: IsParser p => p b1 -> p b2 -> p a -> p a
% pPacked :: CustomErrors
%   '[ '[ p  :/~: p1   :=>: DifferentParsers "pPacked" '[ '(p, 1), '(p1 , 2), '(p2, 3) ]
%       , p1 :/~: p2   :=>: DifferentParsers "pPacked" '[ '(p, 1), '(p1 , 2), '(p2, 3) ]]
%    , '[ Check (IsParser p) ]
%    ] => p b1 -> p1 b2 -> p2 a -> p a
% pPacked = Derived.pPacked
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
% -- pList    ::    IsParser p => p a -> p [a]
% -- pList         p =  must_be_non_empty "pList"    p (pFoldr        list_alg   p)
% -- pList_ng ::    IsParser p => p a -> p [a]
% -- pList_ng      p =  must_be_non_empty "pList_ng" p (pFoldr_ng     list_alg   p)
%
% -- pList1    ::  IsParser p =>  p a -> p [a]
% -- pList1         p =  must_be_non_empty "pList"    p (pFoldr1       list_alg   p)
% -- pList1_ng ::   IsParser p => p a -> p [a]
% -- pList1_ng      p =  must_be_non_empty "pList_ng" p (pFoldr1_ng    list_alg   p)
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
% -- | `pExact` recognises a specified number of elements
% pExact :: (IsParser f) => Int -> f a -> f [a]
% -- pExact :: CustomErrors
% --   '[ '[ int  :/~: Int   :=>:  Text "The first argument to pExact must be the number of elements to be recognised" :<>: Colon
% --       , fa   :/~: f a   :=>:  Text "The second argument to pExact is the parser t:<+>: Text "doesn't seem to be a parser" ]
%    -- , '[ Check (IsParser f) ]
%    -- ] => int -> fa -> f [a]
% pExact = Derived.pExact
%
% -- pBetween :: (IsParser f) => Int -> Int -> f a -> f [a]
% -- pBetween m n p |  n < 0 || m <0 =  error "negative arguments to pBwteeen"
% --                |  m > n         =  empty
% --                |  otherwise     =  (++) <$> pExact m p <*> pAtMost (n-m) p
%
% -- pAtLeast ::  (IsParser f) => Int -> f a -> f [a]
% -- pAtLeast n p  = (++) <$> pExact n p <*> pList p
%
% -- pAtMost ::  (IsParser f) => Int -> f a -> f [a]
% -- pAtMost n p | n > 0  = (:) <$> p <*> pAtMost (n-1) p `opt`  []
% --             | n == 0 = pure []
%
% -- -- * Counting Parser
% -- -- | Count the number of times @p@ has succeeded
% -- pCount :: (IsParser p, Num b) => p a -> p b
% -- pCount p = (\_ b -> b+1) <$> p <*> pCount p  `opt` 0
%
% -- -- * Miscelleneous
% -- -- | Build a parser for each element in the argument list and try them all.
% -- pAny :: IsParser p => (a -> p a1) -> [a] -> p a1
% -- pAny  f l =  foldr (<|>) pFail (map f l)
%
% -- -- | pSym was removed because the class Provides was eliminated
% -- -- pAnySym :: Provides st s s => [s] -> P st s
% -- -- pAnySym = pAny pSym
%
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
                          Text "argument is" :<+>: ShowType p :<>: Colon

 data MakeParserArgSym :: ((k , Nat) ~> ErrorMessage) -> *

 type instance Apply MakeParserArgSym x = MakeParserArg x
\end{code}
