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


\subsection{|pEither|}

The first interesting combinator that can be customized of this module is

%if style /= newcode
\begin{code}
pEither :: IsParser p => p a -> p b -> p (Either a b)
\end{code}
%endif

In this case, we have to check that both argument are parsers or at least look like
parsers and if they are different give the appropiate error message.

\begin{code}
pEither :: CustomErrors
  ![ ![IsNotOfParserKind "pEither" 1 p1 p  a
     ^^,IsNotOfParserKind "pEither" 2 p3 p2 b]
  ^^,![IsNotAParser p  :/~: False :=>: ExpectedErrorMessage "pExact" 1 "a parser" p
     ^^,IsNotAParser p2 :/~: False :=>: ExpectedErrorMessage "pExact" 2 "a parser" p2]
  ^^,![p :/~: p2 :=>: DifferentParsers "pEither" ![ !(p1, 1 ) , !(p3, 2) ]]
  ^^,![Check (IsParser p)]
   ]  =>  p1 -> p3 -> p (Either a b)
pEither = Derived.pEither
\end{code}

\subsection{Infix combinators}

This module provides two infix combinators for parsers with underlying function
types,

%if style /= newcode
\begin{code}
(<$$>) ::  IsParser p => (a -> b -> c) -> p b -> p (a -> c)
(<??>) :: IsParser p => p a -> p (a -> a) -> p a
\end{code}
%endif

The second combinator, |(<??>)| is very similar to the ones we defined in the core
module and up to some extent it can be considered a \sibling of them. However, it is
different in the sense that the plain parser type |p a| occurs in the first argument as
opossed to those other combinators in \ref{subsec:Functor}. Unlike those other combinators, the
error message of this type will not be biased towards any of its arguments and only in case the
underlying arrow type if the second argument is not as expected we will make a suggestion.

\begin{code}
(<??>) ::
  CustomErrors
    ![ ![ IsNotOfParserKind "(<??>)" 1 p2 p1 a1
        , IsNotOfParserKind "(<??>)" 1 p4 p3 f]
     , ![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "(<??>)" 1 "a parser" p2
        , IsNotAParser p3 :/~: False :=>: ExpectedErrorMessage "(<??>)" 1 "a parser" p4]
     , ![p1 :/~: p3 :=>: DifferentParsers "(<??>)" ![ !(p2, 1 ) , !(p4, 2) ]]
     , ![ f :/~: (a2 -> a2) :=?>:
          !( ![ f :~?: a1 :=!>:
                VSep ![Text "In the application of '(<??>)', the underlying type of the #1 argument parser"
                        :$$: Quote (ShowType p2) :<>: Comma
                     ^^,Indent 2 (ShowType a1)
                     ^^,Text "is not a function. But the underlying type of the parser in the #2 argument,"
                        :$$: Quote (ShowType p4) :<>: Comma
                     ^^,Indent 2 (ShowType a1)
                     ^^,Text "matches." :$$: Text "Maybe you intended to use  '(<|>)' or '(<<|>)'?"]]
          ,   FunctionTypeParserEq 2 f 1)]
   , ![ a1 :/~: a2        :=>:
          VSep  ![Text "In the application of '(<??>)', the underlying type of the #1 argument parser"
                     :<+>: Quote (ShowType p2) :<>: Comma
                ^^,Indent 2 (ShowType a1)
                ^^,Text "and the type of source and target of the function inside the #2 argument parser,"
                ^^,Indent 2 (ShowType f)
                ^^,Text "have to match." ]]
   , ![ Check (IsParser p1) ]
   ] => p2 -> p3 f -> p1 a1
(<??>)         = (Derived.<??>)
\end{code}

The other combinator |<$$>| is much more easy to customize, as we can check independently the first
argument to be a function and the second to be a parser.

\begin{code}
(<$$>) :: CustomErrors
    ![ ![ IsNotOfParserKind "(<$$>)" 2 p2 p b
        , f :/~: (a -> b1-> c) :=>: FunctionTypeParser 1 f 2]
   , ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage "(<$$>)" 2 "a parser" p2]
   , ![ b1 :/~: b  :=>:
          VSep  ![Text "In the application of '(<$$>)', the underlying type of the #2 parser argument"
                   :<+>: Quote (ShowType p2)
                ^^,Indent 2 (ShowType b)
                ^^,Text "and the type of the #2 argument of the function given as #1 argument,"
                ^^,Indent 2 (ShowType f)
                ^^,Text "have to match." ]]
   , ![ Check (IsParser p) ]
   ] => f -> p b -> p (a -> c)
(<$$>) = (Derived.<$$>)
\end{code}

\subsection{Function composition}

The follwowing two combinators, show a similarity between their types.
%if style /= newcode
\begin{code}
(<.>)  :: IsParser p => p (b -> c) -> p (a -> b) -> p (a -> c)
(<..>) :: IsParser p => p (a -> b) -> p (b -> c) -> p (a -> c)
\end{code}
%endif

In order to customize the error message we have to check that both arguments
are parser like arguments, and they hold a function type inside. As a last step
we should make sure the source/target of the arrow matches as expected.

When the types do not match as expected we can suggest the user that maybe
he intended to use the other.

\begin{code}

type CompositionError (name :: Symbol) (sug :: Symbol) p p1 p2 p3 f1 f2 a b1 b2 c tf1 tf2 =
 CustomErrors
    ![ ![ IsNotOfParserKind name 1 p1 p  f1
        , IsNotOfParserKind name 2 p3 p2 f2]
     , ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage name 1 "a parser" p
        ,  IsNotAParser p2 :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p2]
     , ![p :/~: p2 :=>: DifferentParsers "pEither" ![ !(p1, 1 ) , !(p3, 2) ]]
     , ![ f1 :/~: tf1   :=>: FunctionTypeParser 1 f1 1
        , f2 :/~: tf2   :=>: FunctionTypeParser 2 f2 1]
   ,![ b1 :/~: b2   :=?>:
        !( ![c :~?: a :=!>:
              VCat  ![Text "The target type of the #2 function,"
                    ^^,Indent 2 (ShowType b2)
                    ^^,Text "and the source type of the #1 function,"
                    ^^,Indent 2 (ShowType b1)
                    ^^,Text "should match."
                    ^^,Text "Maybe you wanted to use" :<+>: Quote (Text sug) :<+>: Text "?"]]
         ,VCat  ![Text "In the use of" :<+>: Quote (Text name) :<+>: Text "the target type of the #2 function,"
                ^^,Indent 2 (ShowType b2)
                ^^,Text "and the source type of the #1 function,"
                ^^,Indent 2 (ShowType b1)
                ^^,Text "should match."])]
   , ![ Check (IsParser p) ]
   ]

(<.>) :: CompositionError "<.>" "<..>" p p1 p2 p3 f1 f2 a b1 b2 c (b1 -> c) (a -> b2)
      => p1 -> p3 -> p (a -> c)
(<.>) = (Derived.<.>)

(<..>) :: CompositionError "<..>" "<.>" p p1 p2 p3 f1 f2 a b1 b2 c (a -> b1) (b2 -> c)
      => p1 -> p3 -> p (a -> c)
(<..>) = (Derived.<..>)
\end{code}

Maybe the abstraction of the custom error to the |CompositionError| type synonym is a little bit
forced as we must include as the last two arguments the relation that must hold for each function
type between the type variables |a b1 b2 c|.

\subsection{List with separation parsers}

For the familiy of separation parsers the error is quite straightforward to customize. We should
make sure that both arguments are parser like arguments and finally the underlying typ
has to match.

\begin{code}
type PListSep (name :: Symbol) = forall p p1 p2 p3 a b.
  CustomErrors
    ![ ![ IsNotOfParserKind name 1 p1 p  a
        , IsNotOfParserKind name 2 p3 p2 b]
     ,  ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage name 1 "a parser" p
        ,  IsNotAParser p2 :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p2]
     , ![ p  :/~: p2   :=>: DifferentParsers name ![ !(p , 1) , !(p2 , 2)]]
     , ![ a :/~: b     :=>: VSep ![ Text "The underlying type of both parsers,"
                                  , Indent 2 (Quote (ShowType p1) :<+>: Text "and" :<+>: Quote (ShowType p3))
                                  , Text "does not match."]]
     , ![ Check (IsParser p) ]
     ] => p1 -> p3 -> p [a]

pListSep     :: PListSep "pListSep"
pListSep     = Derived.pListSep

pListSep_ng  :: PListSep "pListSep_ng"
pListSep_ng  = Derived.pListSep_ng

pList1Sep    :: PListSep "pList1Sep"
pList1Sep    = Derived.pList1Sep

pList1Sep_ng :: PListSep "pList1Sep_ng"
pList1Sep_ng = Derived.pList1Sep_ng
\end{code}

\subsection{Chain parsers}

In the combinators for chaining parsers the customized type error is a bit involved.
It must first check that the provided arguments are parser like types. Then the
underlying type of the first parser must be the function used to chain, and it should
be of exactly two arguments that moreover match the type of the second argument parser.

An option to customize the error to this family of combinators would be to check the combinations
of types for both arguments to check which one differs and then give a precise error for it.
Another option, that we choose to follow is to tell the user that indeed we expect a function
type of two arguments with the type |c -> c -> c|. Therefore, in a subsequent step we check if the
|c| matches the underlying type of the second argument parser.

\begin{code}
type PChain (name :: Symbol) = forall p p1 p2 p3 fc c1 c.
  CustomErrors
    ![ ![ IsNotOfParserKind name 1 p1 p fc
        , IsNotOfParserKind name 2 p3 p2 c]
     ,  ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage name 1 "a parser" p
        ,  IsNotAParser p2 :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p2]
     , ![ p  :/~: p2   :=>: DifferentParsers name ![ !(p , 1) , !(p2 , 2)]]
     , ![ fc :/~: (c1 -> c1 -> c1) :=>: FunctionTypeParserEq 1 p1 2 ]
     , ![ c1 :/~: c :=>: VSep ![ Text "The underlying type of the #2 argument parser,"
                               , Indent 2 (Quote (ShowType p3))
                               , Text "has to match the type of arguments and target of the function in the #1 argument,"
                               , Indent 2 (Quote (ShowType p1))
                               ]]
     , ![ Check (IsParser p) ]
     ] => p1 -> p3 -> p c

pChainr :: PChain "pChainr"
pChainr = Derived.pChainr

pChainr_ng :: PChain "pChainr_ng"
pChainr_ng = Derived.pChainr_ng

pChainl :: PChain "pChainl"
pChainl = Derived.pChainl

pChainl_ng :: PChain "pChainl_ng"
pChainl_ng = Derived.pChainl_ng
\end{code}

\subsection{Repeating parsers}

There are some combinators that share a common pattern for repeatedly applying a
given parser a fixed number of times. These are,

%if style /= newcode
\begin{code}
pExact   :: (IsParser f) => Int -> f a -> f [a]
pAtLeast :: (IsParser f) => Int -> f a -> f [a]
pAtMost  :: (IsParser f) => Int -> f a -> f [a]
\end{code}
%endif

For the cutomized error of this family of combinators, we are going to first
check that the second argument is a parser and then that the first one is an
|Int|. In case we find the first one is not a parser but an |Int| we can suggest the user
that maybe he swapped the arguments. The drawback of this approach is that we will make the
suggestion even if the first one is already an |Int| and not a parser. However, there is no
way to encode in the framework this double dependency of the first being a parser and the
second one being an |Int|.

Moreover, this only occurs in the first check to see if the parser argument we expect has the
right kind, this is a parser |p| applied to some type |a|, |p a|. Once we know it has this shape,
we still have to rule out the cases where we know the type is not a parser.

In order to encode all the three cases together we will make use of some type
level machinery.

\begin{code}
type Repeating (name :: Symbol) = forall int p p1 a.
  CustomErrors
    ![ ![ p1 :/~: p a :=?>:
            !( ![int ~ p1 :=!>: Text "The #2 argument is an 'Int', Maybe the arguments are swapped?"]
             ,  ExpectedErrorMessage name 2 "a parser" p1)]
     , ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p1
        , int :/~: Int  :=>: ExpectedErrorMessage name 1 "a 'Int'" int]
      , ![ Check (IsParser p) ]
      ] => int -> p1 -> p [a]
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

For the follwing combinator,

%if style /= newcode
\begin{code}
pBetween :: (IsParser f) => Int -> Int -> f a -> f [a]
\end{code}
%endif

the ideal type error message would be to point out in case the second argument
to the function is of type |f a| that maybe one of the combinators above was the intended
one to use. However, there is no mechanism that allows us to be sure that indeed is a parser
in case the argument is not of |Int| type and therefore we would be misleading the user
with the type error. Because of this we choose to only provide basic type error messages in
case the arguments type do not match what was expected.

\begin{code}
pBetween ::
  CustomErrors
    ![ ![ int1 :/~: Int  :=>: ExpectedErrorMessage "pBetween" 1 "the minimum number of elements 'Int' to be recognised" int1
       ,  int2 :/~: Int  :=>: ExpectedErrorMessage "pBetween" 2 "the minimum number of elements 'Int' to be recognised" int2
        , IsNotOfParserKind name 3 p1 p a]
     , ![ IsNotAParser p  :/~: False :=>: ExpectedErrorMessage name 1 "a parser" p1]
     , ![ Check (IsParser p) ]
     ] => int1 -> int2 -> p1 -> p [a]
pBetween = Derived.pBetween
\end{code}


\subsection{Other combinators}

The customization of the type error message of the follwing combinator,

%if style /= newcode
\begin{code}
pPacked :: IsParser p => p b1 -> p b2 -> p a -> p a
\end{code}
%endif

Makes explicit why checking that the arguments are indeed parser does not scale well
when there are more than two arguments with parser type. First we have to check that all
three arguments have parser like arguments to have some kind of assurement. Then when we
can more safely assume that at least they are parsers we should check that they are the same
parser. However, we can only check a pair each time and because of this we should include
in the error message all given parsers. Moreover, the checking has to be done in different steps
not to prompt the user with more than one error message stating the error.

\begin{code}
pPacked :: CustomErrors
    ![ ![ IsNotOfParserKind "pPacked" 1 p2 p1 b1
        , IsNotOfParserKind "pPacked" 2 p4 p3 b2
        , IsNotOfParserKind "pPacked" 3 p6 p5 a  ]
     , ![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "pPacked" 1 "a parser" p1
        , IsNotAParser p3 :/~: False :=>: ExpectedErrorMessage "pPacked" 2 "a parser" p3
        , IsNotAParser p5 :/~: False :=>: ExpectedErrorMessage "pPacked" 3 "a parser" p5]
     ,  ![ p1  :/~: p3   :=>: DifferentParsers "pPacked" ![ !(p1, 1), !(p3 , 2), !(p5, 3) ]]
     ,  ![ p3  :/~: p5   :=>: DifferentParsers "pPacked" ![ !(p1, 1), !(p3 , 2), !(p5, 3) ]]
     ,  ![ Check (IsParser p1) ]
   ] => p2 -> p4 -> p6 -> p1 a
pPacked = Derived.pPacked
\end{code}

% Uninteresting cases.

%if style == newcode
\begin{code}
pCount :: (IsParser p, Num b) => p a -> p b
pCount = Derived.pCount

pAny :: IsParser p => (a -> p a1) -> [a] -> p a1
pAny  = Derived.pAny

infixl 4  <??>

pMany :: IsParser p => p a -> p [a]
pMany = Derived.pMany

pSome :: (IsParser f) => f a -> f [a]
pSome = Derived.pSome

pReturn :: Applicative p => a -> p  a
pReturn  = Derived.pReturn

pFail :: Alternative  p => p  a
pFail    = Derived.pFail

pMaybe :: IsParser p => p a -> p (Maybe a)
pMaybe   = Derived.pMaybe

pFoldr    :: IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr = Derived.pFoldr

pFoldr_ng ::  IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr_ng = Derived.pFoldr_ng


pFoldr1    :: IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1 = Derived.pFoldr1

pFoldr1_ng ::  IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1_ng = Derived.pFoldr1_ng

pList    :: IsParser p => p a -> p [a]
pList     = Derived.pList

pList_ng :: IsParser p => p a -> p [a]
pList_ng  = Derived.pList_ng

pList1    :: IsParser p =>  p a -> p [a]
pList1     = Derived.pList1
pList1_ng :: IsParser p => p a -> p [a]
pList1_ng  = Derived.pList1_ng

pFoldrSep    ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep = Derived.pFoldrSep

pFoldrSep_ng ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep_ng = Derived.pFoldrSep_ng

pFoldr1Sep    ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep = Derived.pFoldr1Sep

pFoldr1Sep_ng ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep_ng = Derived.pFoldr1Sep_ng

\end{code}
%endif
