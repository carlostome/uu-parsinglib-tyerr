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

import           Text.ParserCombinators.UU.Core (Cost, Eof (..), ExtAlternative,
                                                 HasPosition (..),
                                                 IsLocationUpdatedBy (..),
                                                 IsParser, P (..), Progress,
                                                 Steps (..), StoresErrors (..),
                                                 Strings)
import qualified Text.ParserCombinators.UU.Core as Core

import           GHC.TypeErrors
import           GHC.TypeErrors.PP
import           GHC.TypeErrors.Utils
import           GHC.TypeLits

import           Control.Applicative            (Alternative)
import qualified Control.Applicative            as Applicative
import           Prelude                        hiding ((*>), (<$>), (<*),
                                                 (<*>), (<|>))
import           Prelude                        (Applicative, Functor, Int,
                                                 Maybe, String)
\end{code}
%endif

\subsection{|Functor|, |Applicative|, |Alternative| and |ExtAlternative|}
\label{subsec:Functor}

Many of the functionality provided by this parser library comes from the use of
|Functor|, |Applicative| and |Alternative| type classes defined in Haskell
standard library. Instead of redefining the classes to give custom error we
define the same combinators they offer but enhanced with custom error messages.

The disadvantage of this approach is that the user has to be careful to use the ones
defined here and not the provided ones by |Prelude|.

Moreover, uu-parsinglib defines the type class |ExtAlternative| which is meant to
provide greedy versions of the parsers that can be built from the Haskell
|Alternative| typeclass.

To customize the type errors of the methods of this class we will export the
original class but customized versions of the combinators it offer.

Therefore, the combinators that we will customize here are,


%if style /= newcode
\begin{code}
(<$>)  :: Functor f        => (a -> b)   -> f b    -> f a
(<*>)  :: Applicative f    => f (a -> b) -> f a    -> f b
(<*)   :: Applicative f    => f a        -> f b    -> f a
(*>)   :: Applicative f    => f a        -> f b    -> f b
(<|>)  :: Alternative f    => f a        -> f a    -> f a
(<<|>) :: ExtAlternative p => p a        -> p a    -> p a
(<?>)  :: ExtAlternative p => p a        -> String -> p a

must_be_non_empty   :: ExtAlternative p => String -> p a ->        c -> c
must_be_non_empties :: ExtAlternative p => String -> p a -> p b -> c -> c
\end{code}
%endif

By looking at their type, we can understand that all of them share an almost
identical type. Except for the last two function the identifiers for rest of
them are very similar and can be mistakenly interchanged in their use.

Because the difference is most significant in the type of the first argument
(except for |(<?>)|),  we will customize the type errors to be biased towards
identifying first the second argument being a parser |p| applied to some type
|a|, and then regarding the possible type errors that derivate from the first
argument suggest other functions in the list as possible solutions.

However, there is a drawback with this approach. Within the type error
framework we are not able to know with certainty that at a given point in
the error message a type |p| is indeed a parser. For example, in the following
type signature,


%if style /= newcode
\begin{code}
CustomErrors
  ![p1 :/~: p a :=>: ExpectedErrorMessage "(<$>)" 2 "a parser" p1
  ^^,![ Check (IsParser p)]
   , ...
   ] => p1 -> ...
\end{code}
%endif

At ... , we would like that we have certainty of |p| being a parser.  However,
the semantics of the combinators for customizing error messages do not ensure
us that this is the case.

Because of such limitation, we can make use of type families that
will help us to discard the cases where we are sure the argument cannot be a
parser.  If the type of |p1| cannot be decomposed into some |p| applied to |a|
then it is not a parser, maybe is some type of kind |*| such as Int, String,
etc.

But if the type can be decomposed, then we must make sure is not of some type
that we know it is not an instance of |Parser|.  For example, |p = ((->) b)|
has the right kind but it is not a parser.

This are defined in \ref{sec:Utils} as |IsNotOfParserKind| and |IsNotAParser|.
The former checks the condition of being of the appropriate shape and the later
discards cases we know are not parsers.

Now that we have some kind of assurance (even not that much) that the second
argument to the function looks like a parser, we can check the second argument
and prompt the user with useful hints about the possible \siblings he intended
to use in case there is a type error.

For example, in a call to |(<$>)| that makes use of a first argument of
type |p (a -> b)| we can be pretty sure that the user intended to use |(<*>)|
instead. However, this interpretation only holds if we already know that the
second argument is of type |p a| for a parser |p|. Therefore, we delay the check
of the first argument to a second place to be able to make such suggestion.

If instead the type of the first argument turns out to be  |p a| then we should 
suggest that the user maybe wanted to use either |(<|>)| or |(<<|>)|.
As a last resource if the underlying types do not match we can still ask the
user if he intended to use |(<*)| or |(*>)|.

It is important to remark that if the first type is not a parser but a |String|
then we won't suggest |(<?>)| as a \sibling. This is because we cannot encode
nested conditions within the type error DSL.

\begin{code}
(<$>) ::
  CustomErrors
    ![ ![ IsNotOfParserKind "(<$>)" 2 p2 p a1 ]
    ^^,![ IsNotAParser p :/~: False :=>: ExpectedErrorMessage "(<$>)" 2 "a parser" p2]
    ^^,![ f1  :/~: (a -> b) :=?>:
            !( ![ f1 :~?: p (a1 -> b) :=!>:
                    VSep  ![Text "The #1 argument to '(<$>)' is a function wrapped in a parser:"
                          ^^,Indent 2 (ShowType f1)
                          ^^,Text "Maybe you pretended to use '(<*>)'?" ]
                , f1 :~?: p a1 :=!>:
                    VSep  ![Text "The #1 argument to '(<$>)' is a parser not a plain function:"
                          ^^,Indent 2 (ShowType f1)
                          ^^,Text "It matches the parser type of the #2 argument." :$$:
                                Text "Maybe you pretended to use '(<|>)' or '(<<|>)'?" ]
                , f1 :~?: p c :=!>:
                    VSep  ![Text "The #1 argument to '(<$>)' is a parser not a plain function:"
                          ^^,Indent 2 (ShowType f1)
                          ^^,Text "Maybe you pretended to use '(<*)' or '(*>)'?" ]]
              , ExpectedErrorMessage "(<$>)" 1 "a function of at least 1 argument" f1)]
    ^^,![ a :/~: a1 :=>:
          VSep  ![Text "In the application of '(<$>)', the source type of the function in the #1 argument"
                    :$$: Quote (ShowType f1) :<>: Comma
                ^^,Indent 2 (ShowType a)
                ^^,Text "and the underlying type of the parser in the #2 argument,"
                      :$$: Quote (ShowType p2) :<>: Comma
                ^^,Indent 2 (ShowType a1)
                ^^,Text "should match."]]
    ^^,![ Check (IsParser p) ]
    ] => f1 -> p a1 -> p b
(<$>) = (Applicative.<$>)

(<*>) ::
  CustomErrors
    ![ ![ IsNotOfParserKind "(<*>)" 2 p2 p1 a1 ]
    ^^,![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "(<*>)" 2 "a parser" p1]
    ^^,![ IsNotOfParserKind "(<*>)" 1 p4 p3 f1 ]
    ^^,![ IsNotAParser p3 :/~: False :=?>:
            !( ![ p4 :~?: (a1 -> b) :=!>:
                    VSep  ![Text "The #1 argument to '(<*>)' is a plain function,"
                          ^^,Indent 2 (ShowType p4)
                          ^^,Text "but it should be wrapped on a parser as,"
                          ^^,Indent 2 (ShowType (p1 p4))
                          ^^,Text "Maybe you pretended to use '(<$>)'?" ]]
              , ExpectedErrorMessage "(<*>)" 1 "a parser with a function type of at least 1 argument" p3)]
    ^^,![p1 :/~: p3 :=>: DifferentParsers "(<*>)" ![ !(p1, 2 ) , !(p3, 1) ]]
    ^^,![ f1 :/~: (a -> b) :=?>:
            !( ![ f1 :~?: a1 :=!>:
                    VSep  ![Text "The #1 argument to '(<$>)' is a parser not a plain function:"
                          ^^,Indent 2 (ShowType f1)
                          ^^,Text "It matches the parser type of the #2 argument." :$$:
                               Text "Maybe you pretended to use '(<|>)' or '(<<|>)'?" ]]
              ,      VSep  ![Text "The #1 argument to '(<$>)' is a parser without an underlying function:"
                           ^^,Indent 2 (ShowType p4)
                           ^^,Text "Maybe you pretended to use '(<*)' or '(*>)'?" ])]
    ^^,![ a :/~: a1 :=>:
          VSep  ![Text "In the application of '(<*>)', the source type of the function in the #1 argument"
                    :$$: Quote (ShowType f1) :<>: Comma
                ^^,Indent 2 (ShowType a1)
                ^^,Text "and the underlying type of the parser of the #2 argument,"
                      :<+>: Quote (ShowType p1)
                ^^,Indent 2 (ShowType a)
                ^^,Text "should match."]]
    ^^,![ Check (IsParser p1) ]
     ] => p4 -> p2 -> p1 b
(<*>) = (Applicative.<*>)
\end{code}

For the functions |(<*)|, |(*>)| it is not possible to propose |(<*>)| as a
\sibling because if the first parser has a function type inside is still well typed. However,
for the case of the first argument being a plain function we can suggest the user that maybe
he/she wanted to write |(<$>)| instead.

\begin{code}
type ErrorApplicative (name :: Symbol) p1 p2 p3 p4 a b c = CustomErrors
    ![ ![ IsNotOfParserKind name 2 p2 p1 a ]
    ^^,![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p1]
    ^^,![ IsNotOfParserKind "(<*)" 1 p4 p3 b ]
    ^^,![ IsNotAParser p3 :/~: False :=?>:
            !( ![ p4 :~?: (a -> c) :=!>:
                    VSep  ![Text "The #1 argument to" :<+>: Quote (Text name) 
                              :<+>: Text "is a plain function,"
                          ^^,Indent 2 (ShowType p4)
                          ^^,Text "but it should be a parser type." :$$:
                               Text "Maybe you pretended to use '(<$>)'?"]]
            ^^,ExpectedErrorMessage name 1 "a parser" p4)]
    ^^,![ p1 :/~: p3 :=>: DifferentParsers name ![ !(p1, 2 ) , !(p3, 1) ]]
    ^^,![ Check (IsParser p1) ]
     ]

(<*) :: ErrorApplicative "(<*)" p1 p2 p3 p4 a b c => p4 -> p2 -> p1 b
(<*)  = (Applicative.<*)

(*>) :: ErrorApplicative "(*>)" p1 p2 p3 p4 a b c => p4 -> p2 -> p1 a
(*>)  = (Applicative.*>)
\end{code}

For the combinators from |Alternative| and |ExtAlternative| , once we can assure with some certainty that the
second argument is of type |p a| for some parser |p| and an argument a, we can proceed to
check the type of the first argument. If its type is a plain function not wrapped in a parser
then we should suggest |<$>| as \sibling.

In the case it is a parser but the underlying type is a function such that the source type matches
|a| then we should suggest |(<*>)| as a \sibling. In the remaining case if the underlying type is
not a function and doesn't match |a| then we can suggest that maybe the intention was to use either
|(<*)| or |(*>)|.

\begin{code}
type ErrorAlternative (name :: Symbol) p1 p2 p3 p4 a b c = CustomErrors
    ![ ![ IsNotOfParserKind name 2 p2 p1 a ]
    ^^,![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage name 2 "a parser" p1]
    ^^,![ IsNotOfParserKind "(<|>)" 1 p4 p3 b ]
    ^^,![ IsNotAParser p3 :/~: False :=?>:
            !( ![ p4 :~?: (a -> c) :=!>:
                    VSep  ![Text "The #1 argument to" :<+>: Quote (Text name)
                              :<+>: Text  "is a plain function,"
                          ^^,Indent 2 (ShowType p4)
                          ^^,Text "but it should be a parser type." :$$:
                              Text "Maybe you pretended to use '(<$>)'?"]]
            ^^,ExpectedErrorMessage name 1 "a parser" p4)]
     ^^,![ p1 :/~: p3 :=>: DifferentParsers "(<|>)" ![ !(p1, 2 ) , !(p3, 1) ]]
     ^^,![ a :/~: b   :=?>:
            !( ![ b :~?: (a -> c) :=!>:
                    VSep  ![Text "The #1 argument to" :<+>: Quote (Text name)
                              :<+>: Text "is a parser with an underlying function type,"
                          ^^,Indent 2 (ShowType p4)
                          ^^,Text "but it should match the #2 argument parser type." :$$:
                                Text "Maybe you pretended to use '(<*>)'?"]]
            , Text "Don")]
     ^^,![ Check (IsParser p1) ]
      ]

(<|>) :: ErrorAlternative "(<|>)" p1 p2 p3 p4 a b c   => p4 -> p2 -> p1 a
(<|>) = (Applicative.<|>)

(<<|>) :: ErrorAlternative "(<<|>)" p1 p2 p3 p4 a b c => p4 -> p2 -> p1 a
(<<|>) = (Core.<<|>)
\end{code}

For the combinator |(<?>)| we can check in paralel the second argument to be of
type |String| and the first one to be a parser like. In order to be able to
suggest corrections in case the second argument is not a |String| we are going
to bias the type error message towards the first argument being a parser.

In this way, if the second is also a parser we can suggest one of the above
combinators. However, we cannot conditionally check in case the second argument is not a
|String| if the first one is a parser with an underlying function type to
suggest |(<*>)|.

\begin{code}
(<?>)  :: CustomErrors
  ![ ![ IsNotOfParserKind "(<?>)" 1 p2 p1 a ]
  ^^,![IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "(<|>)" 2 "a parser" p1]
  ^^,![str :/~: String :=?>:
          !( ![str :~?: p1 a :=!>:
                  VCat  ![Text "The #2 argument to (<?>) is a parser and not a String."
                        ^^,Text "Maybe you wanted to use '(<<|>)' or '(<|>)'?"]
             ^^,str :~?: p1 b :=!>:
                  VCat  ![ Text "The #2 argument to (<?>) is a parser and not a String."
                        ^^,Text "Maybe you wanted to use '(<*)' or '(*>)'?"]]
          ^^,ExpectedErrorMessage "<?>" 2 "a String" str)]
  ^^,![Check (IsParser p1) ]
   ] => p2 -> str -> p1 a
(<?>) = (Core.<?>)
\end{code}

The last two combinators are not really \siblings of the above, but they are
between them. We can see that their type is similar except that one takes an
extra argument.

\begin{code}
must_be_non_empty :: CustomErrors
  ![ ![ IsNotOfParserKind "must_be_non_empty" 2 p2 p1 a
  ^^,str :/~: String :=>: ExpectedErrorMessage "must_be_non_empty" 1 "a String for the error message" str]
  ^^,![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "must_be_non_empty" 2 "a parser" p1]
  ^^,![ cf :/~: (c -> c) :=?>:
         !( ![ cf :~?: (p1 b -> c -> c) :=!>:
                VCat ![ Text "One argument extra given to must_be_non_empty,"
                     ^^,Text "Maybe you wanted to use 'must_be_non_empties'?"]]
         ^^,ExpectedErrorMessage "must_be_non_empty" 3 "an argument" pbc  )]
  ^^,![ Check (IsParser p1) ]
  ] => str -> p2 -> cf
must_be_non_empty   = Core.must_be_non_empty

must_be_non_empties :: CustomErrors
  ![ ![ IsNotOfParserKind "must_be_non_empties" 2 p2 p1 a
      ^^,str :/~: String :=>: ExpectedErrorMessage "must_be_non_empties" 1 "a String for the error message" str]
  ^^,![ IsNotAParser p1 :/~: False :=>: ExpectedErrorMessage "must_be_non_empties" 2 "a parser" p1]
  ^^,![ pbc :/~: (p3 b -> c -> c) :=?>:
        !( ![ pbc :~?: (c -> c) :=!>: VCat ![ Text "Missing argument for must_be_non_empties,"
                                            ^^,Text "Maybe you wanted to use 'must_be_non_empty'?"]]
        ^^,ExpectedErrorMessage "must_be_non_empties" 3 "a parser" pbc  )]
  ^^,![ p1 :/~: p3 :=>: DifferentParsers "(<|>)" ![ !(p1, 2 ) , !(p3, 3) ]]
  ^^,![ Check (IsParser p1) ]
  ] => str -> p2 -> pbc
must_be_non_empties   = Core.must_be_non_empties
\end{code}

As a final note, the |IsParser| typeclass is defined in the module |Core| as a means to be a 
unifying class for the ones supported by the parsers. In our solution we keep the type of all combinators
polymorphic in the parser type |p| as long as is an instance of this class. However, the only ever instance
of the class declared in the library is |P st a|. Maybe it would have been much more easier, with less polymorphic
functions type errors are much more easy to spell, to make all the functions work for only this type. We choose our approach
because is more extensible in the sense that if another parser of |IsParser| is declared our custom type error messages do not need to be changed.

%if style == newcode
\begin{code}
opt                 :: ExtAlternative p => p a ->   a -> p a
opt                 = Core.opt

infix   2  <?>
infixl  3  <<|>
infixl  2 `opt`
\end{code}
%endif

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
                VCat  ![Text "It seems that the #2 argument given to" :<+>: Text name
                      ^^,Text "is a parser" :<+>: Quote (ShowType t)
                      ^^,Text "and the #1 argument's type matches the state for such parser."
                      ^^,Text "Maybe, are the arguments swapped?"]]
          ^^, ExpectedErrorMessage name 1 "a parser" p )]
    ^^,![ t1 :/~: t       :=>: ExpectedErrorMessage name 2 "the state for the parser" t ]
    ^^,![ Check (Eof t) ]
    ] => p -> t -> a

parse :: ParserError "parse"
parse  = Core.parse

parse_h :: ParserError "parse_h"
parse_h = Core.parse_h
\end{code}

\subsection{Various combinators}

Some other combinators present in the module that are not polymorphic in
the parser type and can be very easily customized.

\begin{code}
addLength ::
  CustomErrors
    ![  ![ int :/~: Int   :=>: ExpectedErrorMessage "addLength" 1 "the number of elements to add" int
        ^^, p :/~: P st a :=>: ExpectedErrorMessage "addLength" 2 "a parser" p]
    ] => int -> p -> P st a
addLength = Core.addLength

micro ::
  CustomErrors
    ![  ![ int :/~: Int      :=>: ExpectedErrorMessage "micro" 2 "the cost to add" int
        ^^, p :/~: P state a :=>: ExpectedErrorMessage "micro" 1 "a parser" p]
    ] => p -> int -> P state a
micro = Core.micro
\end{code}

%if style == newcode
\begin{code}
pSymExt ::  (forall a. (token -> state  -> Steps a) -> state -> Steps a) -> Core.Nat -> Maybe token -> P state token
pSymExt = Core.pSymExt

pSwitch :: (st1 -> (st2, st2 -> st1)) -> P st2 a -> P st1 a
pSwitch  = Core.pSwitch
\end{code}
%endif
