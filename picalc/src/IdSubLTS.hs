{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module IdSubLTS where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Identity
import           Control.Monad.Fail
import           PiCalc
import           Unbound.Generics.LocallyNameless
{-
class (Eq a,Monad m) => Constraint m a where
  (==.) :: a -> a -> m ()
  x ==. y  =  guard $ x== y
  (/=.) :: a -> a -> m ()

infix 4 ==.
infix 4 /=.
-}

interactsB (UpB x) (DnB x') = x==x'
interactsB (DnB x) (UpB x') = x==x'
interactsB _       _        = False

extrudesB (UpB (Var x')) x = x==x'
extrudesB (DnB (Var x')) x = x==x'

-- one :: (Fresh m, Alternative m, MonadFail m) => Pr -> m (Act, Pr)
one (Out x y p)    = return (Up x y, p)
one (TauP p)       = return (Tau, p)
one (Match x y p)  = do  guard $ x == y
                         one p
one (Plus p q) = one p <|> one q
one (Par p q)
  =    do  (l, p') <- one p;  return (l, Par p' q)
  <|>  do  (l, q') <- one q;  return (l, Par p q')
  <|>  do  (lp, bp) <- oneb p;  (lq, bq) <- oneb q
           guard $ interactsB lp lq             -- close
           (y, p', q') <- unbind2' bp bq
           return (Tau, Nu(y.\Par p' q'))
  <|>  do  (Up x v, p') <- one p;  (DnB x', (y,q')) <- oneb' q
           guard $ x == x'
           return (Tau, Par p' (subst y v q'))  -- interaction
  <|>  do  (DnB x', (y,p')) <- oneb' p;  (Up x v, q') <- one q
           guard $ x == x'
           return (Tau, Par (subst y v p') q')  -- interaction
one (Nu b)  = do  (x,p) <- unbind b
                  (l@(Up (Var x') (Var y)), p') <- one p
                  guard $ x /= x' && x /= y 
                  return (l, Nu (x.\p'))
one _       = empty

-- oneb :: (Fresh m, Alternative m, MonadFail m) => Pr -> m (ActB, PrB)
oneb (In x p)      = return (DnB x, p)
oneb (Match x y p) | x == y = oneb p
oneb (Plus p q)  = oneb p <|> oneb q
oneb (Par p q)   =     do  (l,(x,p')) <- oneb' p;  return (l, x.\Par p' q)
                 <|>   do  (l,(x,q')) <- oneb' q;  return (l, x.\Par p q')
oneb (Nu b)      =     do  (x,p) <- unbind b
                           (l,(y,p')) <- oneb' p
                           guard $ not (l `extrudesB` x)
                           return (l, y.\Nu (x.\p'))
                 <|>   do  (x,p) <- unbind b
                           (Up l@(Var y) (Var x'),p') <- one p
                           guard $ x == x' && x /= y
                           return (UpB l, x.\p')  -- open
oneb _           = empty

oneb' p = do (l,b) <- oneb p; r <- unbind b; return (l,r)

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
