%if style == newcode
\begin{code}
{-# LANGUAGE  RankNTypes,
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleInstances,
              KindSignatures,
              CPP #-}

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
    amb,
    pErrors,
    pPos,
    pState,
    pEnd,
    pSwitch,
    pSymExt,
    -- ** Calling Parsers
    parse, parse_h,
    -- ** Acessing and updating various components
    getZeroP,
    getOneP,
    addLength,
    -- ** Evaluating the online result
    eval,
    -- ** Re-exported modules
    module Control.Applicative,
    module Control.Monad
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

import Control.Applicative
import Control.Monad
\end{code}
%endif

% -- instance  MonadPlus (P st) where
% --   mzero = empty
% --   mplus = (<|>)


The module we are working on defines in |class (Alternative p) => ExtAlternative p|
a set of combinators useful for combining parsers. We have two options to
customize such combinators with domain specific errors. We could construct
something like |class (ExtAlternative p) => ExtAlternativeTyErr p| with exactly
the same methods as |ExtAlternative| but with custom error messages, or instead
not introduce a new class but just define new method with the same identifier
and the custom error messages.

\begin{code}
(<<|>) :: ExtAlternative p => p a -> p a -> p a
(<<|>) = (Core.<<|>)
(<?>)  :: ExtAlternative p => p a -> String -> p a
(<?>) = (Core.<?>)
must_be_non_empty   :: ExtAlternative p => String -> p a ->        c -> c
must_be_non_empty   = Core.must_be_non_empty
must_be_non_empties :: ExtAlternative p => String -> p a -> p b -> c -> c
must_be_non_empties = Core.must_be_non_empties
opt                 :: ExtAlternative p => p a ->   a -> p a
opt                 = Core.opt

infix   2  <?>
infixl  3  <<|>
infixl  2 `opt`
\end{code}

% -- -- | The class `Eof` contains a function `eof` which is used to check whether we have reached the end of the input and `deletAtEnd`
% -- --   should discard any unconsumed input at the end of a successful parse.
% -- class Eof state where
% --        eof          ::  state   -> Bool
% --        deleteAtEnd  ::  state   -> Maybe (Cost, state)
%
% -- -- | The input state may maintain a location which can be used in generating error messages.
% -- --   Since we do not want to fix our input to be just a @String@ we provide an interface
% -- --   which can be used to advance this location by passing  information about the part recognised. This function is typically
% -- --   called in the `splitState` functions.
%
% -- class Show loc => loc `IsLocationUpdatedBy` str where
% --     advance :: loc -- ^ The current position
% --             -> str -- ^ The part which has been removed from the input
% --             -> loc
%
% -- -- | The class `StoresErrors` is used by the function `pErrors` which retrieves the generated
% -- --  correction steps since the last time it was called.
% -- --
%
% -- class state `StoresErrors`  error | state -> error where
% --   -- | `getErrors` retrieves the correcting steps made since the last time the function was called. The result can,
% --   --    by using it in a monad, be used to control how to proceed with the parsing process.
% --   getErrors :: state -> ([error], state)
%
% -- class state `HasPosition`  pos | state -> pos where
% --   -- | `getPos` retrieves the correcting steps made since the last time the function was called. The result can,
% --   --   by using it as the left hand side of a monadic bind, be used to control how to proceed with the parsing process.
% --   getPos  ::  state -> pos
%
% -- -- | The data type `T` contains three components, all being some form of primitive parser.
% -- --   These components are used in various combinations,
% -- --   depending on whether you are in the right and side operand of a monad,
% -- --   whether you are interested in a result (if not, we use recognisers),
% -- --   and whether you want to have the results in an online way (future parsers), or just prefer to be a bit faster (history parsers)

% -- instance Show (P st a) where
% --   show (P _ nt e n) = "P _ " ++ maybe "Nothing" (const "(Just _)") nt  ++ maybe "Nothing" (const "(Just _)") e ++ " (" ++ show n ++ ") "
%

\begin{code}
getOneP :: P a b -> Maybe (P a b)
getOneP = Core.getOneP

getZeroP :: P t a -> Maybe a
getZeroP = Core.getZeroP

addLength :: Int -> P st a -> P st a
addLength = Core.addLength
\end{code}

%
%
% -- instance   Functor (P  state) where
% --   fmap f   (P  ap np me l)   =  P (fmap f ap) (fmap (fmap f)  np)  (f <$> me)  l
% --   f <$     (P  ap np me l)   =  P (f <$ ap)   (fmap (f <$)    np)  (f <$  me)  l
%
% -- instance   Applicative (P  state) where
% --   P ap np pe pl  <*> ~(P aq nq  qe ql)  = trace'' "<*>"  (mkParser (combine np pe aq nq (<*>) (<$>))       (pe <*> qe)  (nat_add pl ql))
% --   P ap np pe pl  <*  ~(P aq nq  qe ql)  = trace'' "<* "  (mkParser (combine np pe aq nq (<*)  (<$))        (pe <* qe )  (nat_add pl ql))
% --   P ap np pe pl  *>  ~(P aq nq  qe ql)  = trace'' " *>"  (mkParser (combine np pe aq nq (*>) (flip const)) (pe *> qe )  (nat_add pl ql))
% --   pure a                                = trace'' "pure" (mkParser Nothing (Just a)  Zero)
%
% -- instance Alternative (P   state) where
% --   (P ap np  pe pl) <|> (P aq nq qe ql)
% --     =  let pl' = maybe pl (const Zero) pe
% --            ql' = maybe ql (const Zero) qe
% --            (rl', b) = trace' "calling natMin from <|>" (nat_min pl' ql' 0)
% --            (rl, _)  = nat_min pl ql 0
% --            Nothing `alt` q  = q
% --            p       `alt` Nothing = p
% --            Just p  `alt` Just q  = Just (p <|> q)
% --        in  mkParser ((if b then  id  else flip) alt np nq) (pe <|> qe) rl
% --   empty  = mkParser empty empty  Infinite
%
% -- instance ExtAlternative (P st) where
% --   ~(P ap np pe pl) <<|> ~(P aq nq qe ql)
% --     = let pl' = maybe pl (const Zero) pe
% --           ql' = maybe ql (const Zero) qe
% --           (rl', b) = nat_min pl' ql' 0
% --           (rl, _)  = nat_min  pl  ql  0
% --           bestx :: Steps a -> Steps a -> Steps a
% --           bestx = (if b then id else flip) best
% --           choose:: T st a -> T st a -> T st a
% --           choose  (T ph pf pr)  (T qh qf qr)
% --              = T  (\ k st -> let left  = norm (ph k st)
% --                              in if has_success left then left else left `bestx` qh k st)
% --                   (\ k st -> let left  = norm (pf k st)
% --                              in if has_success left then left else left `bestx` qf k st)
% --                   (\ k st -> let left  = norm (pr k st)
% --                              in if has_success left then left else left  `bestx` qr k st)
% --       in   P (choose  ap aq )
% --              (maybe np (\nqq -> maybe nq (\npp -> return( choose  npp nqq)) np) nq)
% --              (pe <|> qe) -- due to the way Maybe is instance of Alternative  the left hand operator gets priority
% --              rl
% --   P  _  np  pe pl <?> label = let replaceExpected :: Steps a -> Steps a
% --                                   replaceExpected (Fail _ c) = (Fail [label] c)
% --                                   replaceExpected others     = others
% --                                   nnp = case np of Nothing -> Nothing
% --                                                    Just ((T ph pf  pr)) -> Just(T ( \ k inp -> replaceExpected (norm  ( ph k inp)))
% --                                                                                   ( \ k inp -> replaceExpected (norm  ( pf k inp)))
% --                                                                                   ( \ k inp -> replaceExpected (norm  ( pr k inp))))
% --                                 in mkParser nnp pe pl
% --   must_be_non_empty msg p@(P _ _ (Just _)  _) _
% --             = error ("The combinator " ++ msg ++  " requires that its argument cannot recognise the empty string\n")
% --   must_be_non_empty _ _      q  = q
% --   must_be_non_empties  msg (P _ _ (Just _) _) (P _ _ (Just _) _) _
% --             = error ("The combinator " ++ msg ++  " requires that not both arguments can recognise the empty string\n")
% --   must_be_non_empties  _ _ _ q  = q
%
% -- instance IsParser (P st)
%
% -- -- !! do not move the P constructor behind choices/patern matches
% -- instance  Monad (P st) where
% --        p@(P  ap np pe pl ) >>=  a2q =
% --           (P newap newnp  newep (nat_add pl Hole))
% --           where (newep, newnp, newap) = case pe of
% --                                  Nothing -> (Nothing, t, maybe empty id t)
% --                                  Just a  -> let  P aq nq  eq lq = a2q a
% --                                             in  (eq, combine t nq , t `alt` aq)
% --                 Nothing  `alt` q    = q
% --                 Just p   `alt` q    = p <|> q
% --                 t = fmap (\  (T h _ _  ) ->      (T  (  \k -> h (\ a -> unParser_h (a2q a) k))
% --                                                      (  \k -> h (\ a -> unParser_f (a2q a) k))
% --                                                      (  \k -> h (\ a -> unParser_r (a2q a) k))) ) np
% --                 combine Nothing     Nothing     = Nothing
% --                 combine l@(Just _ ) Nothing     =  l
% --                 combine Nothing     r@(Just _ ) =  r
% --                 combine (Just l)    (Just r)    = Just (l <|> r)
% --                 -- | `unParser_h` retreives the history parser from the descriptor
% --                 unParser_h :: P b a -> (a -> b -> Steps r) -> b -> Steps r
% --                 unParser_h (P (T  h   _  _ ) _ _ _ )  =  h
% --                 -- | `unParser_f` retreives the future parser from the descriptor
% --                 unParser_f :: P b a -> (b -> Steps r) -> b -> Steps (a, r)
% --                 unParser_f (P (T  _   f  _ ) _ _ _ )  =  f
% --                 -- | `unParser_r` retreives therecogniser from the descriptor
% --                 unParser_r :: P b a -> (b -> Steps r) -> b -> Steps r
% --                 unParser_r (P (T  _   _  r ) _ _ _ )  =  r
% --        return  = pure
%

\begin{code}
pSymExt ::  (forall a. (token -> state  -> Steps a) -> state -> Steps a) -> Core.Nat -> Maybe token -> P state token
pSymExt = Core.pSymExt

micro :: P state a -> Int -> P state a
micro = Core.micro

amb :: P st a -> P st [a]
amb = Core.amb

pErrors :: StoresErrors st error => P st [error]
pErrors = Core.pErrors

pPos :: HasPosition st pos => P st pos
pPos = Core.pPos

pState :: P st st
pState = Core.pState


pEnd    :: (StoresErrors st error, Eof st) => P st [error]
pEnd    = Core.pEnd

pSwitch :: (st1 -> (st2, st2 -> st1)) -> P st2 a -> P st1 a -- we require let (n,f) = split st in f n to be equal to st
pSwitch  = Core.pSwitch


parse :: (Eof t) => P t a -> t -> a
parse   = Core.parse

parse_h :: (Eof t) => P t a -> t -> a
parse_h = Core.parse_h

eval :: Steps   a      ->  a
eval = Core.eval
\end{code}

