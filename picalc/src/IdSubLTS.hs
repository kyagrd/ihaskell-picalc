{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module IdSubLTS where

import Control.Applicative
import Control.Lens.Fold
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Identity
import Data.Typeable
import PiCalc
import Unbound.Generics.LocallyNameless hiding (fv)
import MemoUgly
-- umemo = id

interactsB (UpB x) (DnB x') = x == x'
interactsB (DnB x) (UpB x') = x == x'
interactsB _ _ = False

sone  p = _one  p
soneb p = _oneb p

_one = umemo ((fmap simplify <$>) . one)
_oneb = umemo ((fmap simplify <$>) . oneb)

one (Out x y p) = return (Up x y, p)
one (TauP p)    = return (Tau, p)
one (Match x y p) =
      do guard $ x == y
         _one p
one (Plus p q) = _one p <|> _one q
one (Par p q) =
      do (l, p') <- _one p; return (l, Par p' q)
  <|> do (l, q') <- _one q; return (l, Par p q')
  <|> do (lp, bp) <- _oneb p
         (lq, bq) <- _oneb q
         guard $ interactsB lp lq -- close
         (y, p', q') <- unbind2' bp bq
         return (Tau, Nu (y .\ Par p' q'))
  <|> do (Up x v, p') <- _one p
         (DnB x', (y, q')) <- onebu q
         guard $ x == x'
         return (Tau, Par p' (subst y v q')) -- interaction
  <|> do (DnB x', (y, p')) <- onebu p
         (Up x v, q') <- _one q
         guard $ x == x'
         return (Tau, Par (subst y v p') q') -- interaction
one (Nu b) =
      do (x, p) <- unbind b
         (l, p') <- _one p
         guard $ x `notElem` fv l
         return (l, Nu (x .\ p'))
one _ = empty

oneb (In x p) = return (DnB x, p)
oneb (Match x y p) =
      do guard $ x == y
         _oneb p
oneb (Plus p q) = _oneb p <|> _oneb q
oneb (Par p q) =
      do (l, (x, p')) <- onebu p; return (l, x .\ Par p' q)
  <|> do (l, (x, q')) <- onebu q; return (l, x .\ Par p q')
oneb (Nu b) =
      do (x, p) <- unbind b
         (l, (y, p')) <- onebu p
         guard $ x `notElem` fv l
         return (l, y .\ Nu (x .\ p'))
  <|> do (x, p) <- unbind b
         (l@(Up (Var y) (Var x')), p') <- _one p
         guard $ x /= y && x == x'
         return (UpB (Var y), x .\ p') -- open
oneb _ = empty

onebu p = do (l, b) <- _oneb p; r <- unbind b; return (l, r)
{-
% Finite pi-calculus specification in lambda-Prolog
% A specification of the late transition system for the finite pi calculus.
% bound input
oneb (in X M) (dn X) M.
% free output
one (out X Y P) (up X Y) P.
% tau
one  (taup P) tau P.
% match prefix
one  (match X X P) A Q :- one  P A Q.
oneb (match X X P) A M :- oneb P A M.
% sum
one  (plus P Q) A R :- one  P A R.
one  (plus P Q) A R :- one  Q A R.
oneb (plus P Q) A M :- oneb P A M.
oneb (plus P Q) A M :- oneb Q A M.
% par
one  (par P Q) A (par P1 Q) :- one P A P1.
one  (par P Q) A (par P Q1) :- one Q A Q1.
oneb (par P Q) A (x\par (M x) Q) :- oneb P A M.
oneb (par P Q) A (x\par P (N x)) :- oneb Q A N.
% restriction
one  (nu x\P x) A (nu x\Q x)      :- pi x\ one  (P x) A (Q x).
oneb (nu x\P x) A (y\ nu x\Q x y) :- pi x\ oneb (P x) A (y\ Q x y).
% open
oneb (nu x\M x) (up X) N :- pi y\ one (M y) (up X y) (N y).
% close
one (par P Q) tau (nu y\ par (M y) (N y)) :- oneb P (dn X) M , oneb Q (up X) N.
one (par P Q) tau (nu y\ par (M y) (N y)) :- oneb P (up X) M , oneb Q (dn X) N.
% comm
one (par P Q) tau (par (M Y) T) :-  oneb P (dn X) M, one Q (up X Y) T.
one (par P Q) tau (par R (M Y)) :-  oneb Q (dn X) M, one P (up X Y) R.
-}
