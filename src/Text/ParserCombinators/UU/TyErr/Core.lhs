%if style == newcode
\begin{code}
{-# LANGUAGE  RankNTypes,
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleInstances,
              KindSignatures,
              CPP #-}

{-# LANGUAGE  DataKinds,
              TypeOperators,
              TypeFamilies,
              AllowAmbiguousTypes,
              PolyKinds,
              ConstraintKinds #-}
module Text.ParserCombinators.UU.TyErr.Core
  ( -- * Classes
    Core.IsParser,
    Core.ExtAlternative,
    (<<|>), (<?>), must_be_non_empty,
    must_be_non_empties, opt,
    Eof (..),
    IsLocationUpdatedBy (..),
    StoresErrors (..),
    HasPosition (..),
    -- * Types
    -- ** The parser descriptor
    P (..),
    -- ** The progress information
    Steps (..),
    Cost,
    Progress,
    -- ** Auxiliary types
    Core.Nat (..),
    Strings,
    -- * Functions
    -- ** Basic Parsers
    micro,
    Core.amb,
    Core.pErrors,
    Core.pPos,
    Core.pState,
    Core.pEnd,
    pSwitch,
    pSymExt,
    -- ** Calling Parsers
    parse, parse_h,
    -- ** Acessing and updating various components
    Core.getZeroP,
    Core.getOneP,
    addLength,
    -- ** Evaluating the online result
    Core.eval,
    -- ** Re-exported modules
    Applicative,
    Applicative.pure, (<$>), (<*>), (<*), (*>),
    Alternative,
    Applicative.empty, (<|>)
  ) where

import qualified Text.ParserCombinators.UU.Core as Core
import Text.ParserCombinators.UU.Core ( ExtAlternative
                                      , IsParser
                                      , P (..)
                                      , StoresErrors (..)
                                      , HasPosition (..)
                                      , IsLocationUpdatedBy (..)
                                      , Eof (..)
                                      , Steps (..)
                                      , Progress
                                      , Strings
                                      , Cost)

import GHC.TypeErrors
import GHC.TypeLits
import GHC.TypeErrors.Utils
import GHC.TypeErrors.PP

import qualified Control.Applicative as Applicative
import Control.Applicative (Alternative)

import Prelude hiding ((<$>), (<*>), (<*), (*>), (<|>))

import Prelude (Functor, Applicative, String, Int, Maybe)
\end{code}
%endif

\subsection{ExtAlternative class}

uu-parsinglib defines the type class |ExtAlternative| which is meant to
provide greedy versions of the parsers that can be built from the Haskell
|Alternative| type class.

To customize the type errors of the methods of this class we have two options.
We can define a new class with exact the same methods as |ExtAlternative|, but
with custom type errors,  and make |ExtAlternative| a superclass of it.
Or instead just define functions with the same identifiers and make sure the |p|
type arguments are in the |ExtAlternative| class.

We choose to follow the later approach as it seems to be more straightforward,
besides that the information about the customized type class might leak and we
do not want to expose its existence.

Therefore, we customize the following methods of |ExtAlternative|,

%if style /= newcode
\begin{code}
(<<|>) :: ExtAlternative p => p a -> p a -> p a
(<?>)  :: ExtAlternative p => p a -> String -> p a
must_be_non_empty   :: ExtAlternative p => String -> p a ->        c -> c
must_be_non_empties :: ExtAlternative p => String -> p a -> p b -> c -> c
\end{code}
%endif

From the type signature we can understand that both methods | (<<>) | and
|(<?>)| can considered to be \textit{siblings} because they only differ in the
type one argument. Moreover, this is also the case with |must_be_non_empty| and
|must_be_non_empties|.  Therefore, in the customized type error we can hint the
user that maybe he/she intended to use the \textit{sibling}



\begin{code}
(<<|>) :: CustomErrors
  ![ ![ p1 :/~: p a1 :=>: ExpectedErrorMessage "(<<|>)" 1 "parser" p1
      , p2 :/~: p a
          :=?>: !( ![p2 :~?: String :=!>: VCat  ![ Text "The 2nd argument to '(<<|>)' is a String and not a parser."
                                                 , Text "Maybe you wanted to use '(<?>)'?"]]
                           , ExpectedErrorMessage "(<<|>)" 2 "a parser" p2) ]
  ,  ![ a1 :/~: a :=>: VCat ![ Text "The underlying type of the parsers to '(<<|>)' have to match,"
                             , Text "but it doesn't."
                             , Empty
                             , Indent 2 (Quote (ShowType p1) :<+>: Text "against" :<+>: Quote (ShowType p2))
                             , Empty
                             , Indent 2 (Quote (ShowType a1) :<+>: Text "against" :<+>: Quote (ShowType a)) ]]
  ,  ![ Check (ExtAlternative p) ]
  ] => p1 -> p2 -> p a
(<<|>) = (Core.<<|>)

(<?>)  :: CustomErrors
  ![  ![ pa :/~: p a :=>: ExpectedErrorMessage "(<?>)" 1 "parser" pa
       , str :/~: String :=?>:
            !( ![str :~?: pa :=!>:
                    VCat  ![  Text "The 2nd argument to (<?>) is a parser and not a String."
                          ^^, Text "Maybe you wanted to use (<<|>)?"]]
                                , ExpectedErrorMessage "<?>" 2 "a String" str)]
   ,  ![ Check (ExtAlternative p) ]
   ] => pa -> str -> p a
(<?>) = (Core.<?>)

must_be_non_empty :: CustomErrors
  ![  ![  str :/~: String :=>: ExpectedErrorMessage "must_be_non_empty" 1 "a String for the error message" str
      ^^, pa :/~: p a     :=>: ExpectedErrorMessage "must_be_non_empty" 2 "a parser" pa]
  ,   ![ cf :/~: (c -> c) :=?>:
          !( ![ cf :~?: (p b -> c -> c) :=!>: VCat ![ Text "One argument extra given to must_be_non_empty,"
                                              , Text "Maybe you wanted to use must_be_non_empties?"]]
              , ExpectedErrorMessage "must_be_non_empties" 3 "a parser" pbc  )]
  ,   ![ Check (ExtAlternative p) ]
  ] => str -> pa -> cf
must_be_non_empty   = Core.must_be_non_empty

must_be_non_empties :: CustomErrors
  ![  ![  str :/~: String :=>: ExpectedErrorMessage "must_be_non_empties" 1 "a String for the error message" str
      ^^, pa :/~: p a     :=>: ExpectedErrorMessage "must_be_non_empties" 2 "a parser" pa]
  ,   ![ pbc :/~: (p1 b -> c -> c) :=?>:
          !( ![ pbc :~?: (c -> c) :=!>: VCat ![ Text "Missing argument for must_be_non_empties,"
                                              , Text "Maybe you wanted to use must_be_non_empty?"]]
              , ExpectedErrorMessage "must_be_non_empties" 3 "a parser" pbc  )]
  ,   ![ p :/~: p1 :=>: VCat ![Text "The parsers of the 2nd and 3rd argument of must_be_non_empties do not match,"
                              , Indent 2 (Quote (ShowType p) :<+>: Text "against" :<+>: Quote (ShowType p1))]]
  ,   ![ Check (ExtAlternative p) ]
  ] => str -> pa -> pbc
must_be_non_empties   = Core.must_be_non_empties

\end{code}

%if style == newcode
\begin{code}
opt                 :: ExtAlternative p => p a ->   a -> p a
opt                 = Core.opt

infix   2  <?>
infixl  3  <<|>
infixl  2 `opt`
\end{code}
%endif

\subsection{Various combinators}

addLength :: Int -> P st a -> P st a
micro :: P state a -> Int -> P state a

\begin{code}
addLength ::
  CustomErrors
    ![  ![ int :/~: Int   :=>: ExpectedErrorMessage "addLength" 1 "the number of elements to add" int
        ^^, p :/~: P st a :=>: ExpectedErrorMessage "addLength" 2 "a parser" p]
    ] => int -> p -> P st a
addLength = Core.addLength

pSymExt ::  (forall a. (token -> state  -> Steps a) -> state -> Steps a) -> Core.Nat -> Maybe token -> P state token
pSymExt = Core.pSymExt

micro ::
  CustomErrors
    ![  ![ int :/~: Int      :=>: ExpectedErrorMessage "micro" 2 "the cost to add" int
        ^^, p :/~: P state a :=>: ExpectedErrorMessage "micro" 1 "a parser" p]
    ] => p -> int -> P state a
micro = Core.micro

pSwitch :: (st1 -> (st2, st2 -> st1)) -> P st2 a -> P st1 a
pSwitch  = Core.pSwitch
\end{code}

\subsection{Evaluation functions}

%if style /= newcode
\begin{code}
parse   :: (Eof t) => P t a -> t -> a
parse_h :: (Eof t) => P t a -> t -> a
\end{code}
%endif

In order to give a custom error message, we should note that the error for this
functions has to be biased towards the type |P t a|, because if that argument
is not a parser then it doesn't make sense to check whether the second
argument's type |t| matches the parser state.

Moreover, it is a common source of errors to use the evaluator function of a
DSL, in this case parser combinators, and supply the arguments in the wrong
order.  In case the first argument is not a parser, we can still check if the
second argument is a parser and the first argument matches the type for the
state of the parser. In this situation we should suggest to the user that is
very likely the arguments are swapped.

\begin{code}
type ParserError (name :: Symbol) = forall p  t t1 a.
  CustomErrors
    ![ ![ p  :/~: P t1 a  :=?>:
          !( ![t :~?: P p a :=!>:
                VCat  ![Text "It seems that the 2nd argument given to" :<+>: Text name
                      ^^,Text "is a parser" :<+>: Quote (ShowType t)
                      ^^,Text "and the 1st argument's type matches the state for such parser."
                      ^^,Text "Maybe, are the arguments swapped?"]]
          ^^, ExpectedErrorMessage name 1 "a parser" p )]
     , ![ t1 :/~: t       :=>: ExpectedErrorMessage name 2 "the state for the parser" t ]
     , ![ Check (Eof t) ]
     ] => p -> t -> a

parse :: ParserError "parse"
parse  = Core.parse

parse_h :: ParserError "parse_h"
parse_h = Core.parse_h
\end{code}

\subsection{Functor, Applicative and Alternative}

Many of the functionality provided by this parser library comes from the use
of |Functor|, |Applicative| and |Alternative| type classes defined in Haskell
standard library. Instead of redefining the classes to give custom error we
define the same combinators they offer but enhanced with custom error messages.

The disadvantage of this approach is that the user has to hide the
actual methods from this type classes when using the library.

For the case of the infix version of |fmap|, with type |Functor f => (a -> b) -> f a -> f b|
we are going to customize the error to be biased towards the
second argument being a parser. This means that first of all we check this
condition alone, thus enabling to propose as \sibling |(<*>)| in case the
second argument is a function wrapped in the same parser type.

If we had checked at the same level both conditions, the first argument being a
function and the second one being a parser, in case neither conditions where
satisfied but the first argument matched a function wrapped on a parser type we
would give a very strange error message to the user.

\begin{code}
(<$>) ::
  CustomErrors
    ![ ![ p1  :/~: p a       :=>: ExpectedErrorMessage "(<$>)" 2 "a parser" p1]
     , ![ f1  :/~: (a1 -> b) :=?>:
            !( ![ f1 :~?: p (a -> b) :=!>:
                    VSep  ![Text "The 1st argument to '(<$>)' is a function wrapped in a parser,"
                             :$$: Indent 2 (ShowType f1)
                           , Text "Maybe you pretended to use '(<*>)'?" ]]
             , ExpectedErrorMessage "(<$>)" 1 "a function of at least 1 argument" f1)]
     , ![ a :/~: a1 :=>:
          VSep  ![Text "In the application of '(<$>)', the source type of the function in the 1st argument"
                    :$$: Quote (ShowType f1) :<>: Comma
                ^^,Indent 2 (ShowType a1)
                ^^,Text "and the underlying type of the parser in the 2nd argument,"
                     :<+>: Quote (ShowType p1)
                ^^,Indent 2 (ShowType a)
                ^^,Text "should match."]]
     , ![ Check (Functor p) ]
     ] => f1 -> p1 -> p b
(<$>) = (Applicative.<$>)
\end{code}

In the case of the function |(<*>)| from Applicative, we have a similar
situation. We have to bias the error to the second argument being a parser, so
in case the first argument is not a function wrapped in the same type of parser
we can give |(<$>)| as a \sibling.

\begin{code}
(<*>) ::
  CustomErrors
    ![ ![ p1  :/~: p a          :=>: ExpectedErrorMessage "(<*>)" 2 "a parser" p1]
     , ![ f1  :/~: p2 (a1 -> b) :=?>:
            !( ![ f1 :~?: (a -> b) :=!>:
                    VSep  ![Text "The 1st argument to '(<*>)' is a plain function,"
                           ,Indent 2 (ShowType f1)
                           ,Text "but it should be wrapped on a parser as,"
                           ,Indent 2 (ShowType (p f1))
                           ,Text "Maybe you pretended to use '(<$>)'?" ]]
             , ExpectedErrorMessage "(<*>)" 1 "a parser with a function type of at least 1 argument" f1)]
     , ![ a :/~: a1 :=>:
          VSep  ![Text "In the application of '(<*>)', the source type of the function in the 1st argument"
                    :$$: Quote (ShowType f1) :<>: Comma
                ^^,Indent 2 (ShowType a1)
                ^^,Text "and the underlying type of the parser of the 2nd argument,"
                     :<+>: Quote (ShowType p1)
                ^^,Indent 2 (ShowType a)
                ^^,Text "should match."]
        , p2 :/~: p :=>: DifferentParsers "(<*>)" ![ !(p, 1 ) , !(p2, 2) ]]
     , ![ Check (Applicative p) ]
     ] => f1 -> p1 -> p b
(<*>) = (Applicative.<*>)
\end{code}

For the following combinators, the kind of type errors that could be happening
are very similar in structure so we can abstract over them.

type family AppCombErrorMessage (name :: Symbol) (b :: Bool) = 
  AppCombErrorMessage name  
\begin{code}
(<*)  :: Applicative f => f a -> f b -> f a
(<*)  = (Applicative.<*)

(*>)  :: Applicative f => f a -> f b -> f b
(*>)  = (Applicative.*>)

(<|>) :: Alternative f => f a -> f a -> f a
(<|>) = (Applicative.<|>)
\end{code}
