{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module OpenLTS where

import GHC.Generics (Generic)
import Control.Applicative
import Control.Lens.Fold
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Reader
import Data.Partition hiding (empty,rep)
import Data.List
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Partition as P
import PiCalc
import Unbound.Generics.LocallyNameless hiding (fv)
import qualified Unbound.Generics.LocallyNameless as U

fv = toListOf U.fv

type Ctx = [Quan]

data Quan = All Nm | Nab Nm deriving (Eq,Ord,Show,Generic)

quan2nm :: Quan -> Nm
quan2nm (All x) = x
quan2nm (Nab x) = x

alls qs = [x | All x <- qs]
nabs qs = [x | Nab x <- qs]

instance Alpha Quan
instance Subst Tm Quan

type Constraint = Partition Int

part2NmSets sigma = do
  xs <- reversedCtxNames
  return $ S.map (xs!!) <$> P.nontrivialSets sigma

respects :: Constraint -> [Int] -> Bool
respects part ns = (P.rep part <$> ns) == ns -- ns - indices of Nab names

respectful sigma =
  do ns <- mapM indexNm . nabs =<< ask
     guard $ respects sigma ns
     return sigma

reversedCtxNames = reverse <$> ctxNames

ctxNames = asks (map quan2nm)

indexNm :: Monad m => Nm -> ReaderT Ctx m Int
indexNm x = fromJust . elemIndex x <$> reversedCtxNames

-- error when x is not in ctx
-- calling reverse every time is not efficient - refactor later
indexNmWith :: Monad m => Constraint -> Nm -> ReaderT Ctx m Int
indexNmWith sigma x = P.rep sigma <$> indexNm x

joinNm :: MonadPlus m => Nm -> Nm -> ReaderT Ctx m Constraint
joinNm x y = respectful =<< joinElems <$> indexNm x <*> indexNm y <*> pure P.discrete

joinTm :: MonadPlus m => Tm -> Tm -> ReaderT Ctx m Constraint
joinTm (Var x) (Var y) = joinNm x y

joinParts = respectful . fromSets . concatMap nontrivialSets

elemWith sigma x xs =
  do i <- indexNmWith sigma x
     is <- mapM (indexNmWith sigma) xs
     guard $ i `elem` is

notElemWith sigma x xs =
  do i <- indexNmWith sigma x
     is <- mapM (indexNmWith sigma) xs
     guard $ i `notElem` is

equalWith sigma x y = elemWith sigma x [y]
noteqWith sigma x y = notElemWith sigma x [y]

interactsB (UpB x) (DnB x') = joinTm x x'
interactsB (DnB x) (UpB x') = joinTm x x'
interactsB _ _ = empty

extendCtx = (:)

subs ctx sigma = substs [(x, Var y) | i <-[0..length ns-1],
                                       let x = ns !! i,
                                       let y = ns !! P.rep sigma i ]
  where ns = reverse . map quan2nm $ ctx

one (Out x y p) = return (P.empty,(Up x y,p))
one (TauP p)    = return (P.empty,(Tau,p))
one (Match x y p) =
      do sigmaxy <- joinTm x y
         (sigma,r) <- one p
         sigma' <- joinParts [sigmaxy,sigma]
         return (sigma',r)
one (Plus p q) = one p <|> one q
one (Par p q) =
      do (sigma,(l,p')) <- one p; return (sigma,(l,Par p' q))
  <|> do (sigma,(l,q')) <- one q; return (sigma,(l,Par p q'))
  <|> do (sigma_p,(lp,bp)) <- oneb p
         (sigma_q,(lq,bq)) <- oneb q
         sigma <- interactsB lp lq             -- close
         sigma' <- joinParts [sigma,sigma_p,sigma_q]
         (y,p',q') <- unbind2' bp bq
         return (sigma',(Tau,Nu (y .\ Par p' q')))
  <|> do (sigma_p,(Up x v,p')) <- one p
         (sigma_q,(DnB x',(y,q'))) <- oneb' q
         sigma <- joinTm x x'
         sigma' <- joinParts [sigma,sigma_p,sigma_q]
         return (sigma',(Tau,Par p' (subst y v q')))  -- interaction
  <|> do (sigma_p,(DnB x',(y,p'))) <- oneb' p
         (sigma_q,(Up x v,q')) <- one q
         sigma <- joinTm x x'
         sigma' <- joinParts [sigma,sigma_p,sigma_q]
         return (sigma',(Tau,Par (subst y v p') q'))  -- interaction
one (Nu b) =
      do (x,p) <- unbind b
         (sigma,(l,p')) <- local (extendCtx (Nab x)) $
            do ret@(sigma,(l,p')) <- one p
               notElemWith sigma x (fv l)
               return ret
         return (sigma,(l,Nu (x .\ p')))
one _ = empty

oneb (In x p) = return (P.empty,(DnB x,p))
oneb (Match x y p) =
      do sigmaxy <- joinTm x y
         (sigma,r) <- oneb p
         sigma' <- joinParts [sigmaxy,sigma]
         return (sigma',r)
oneb (Plus p q) = oneb p <|> oneb q
oneb (Par p q) =
      do (sigma,(l,(x,p'))) <- oneb' p; return (sigma,(l,x .\ Par p' q))
  <|> do (sigma,(l,(x,q'))) <- oneb' q; return (sigma,(l,x .\ Par p q'))
oneb (Nu b) =
      do (x,p) <- unbind b
         (sigma,(l,(y,p'))) <- local (extendCtx (Nab x)) $
           do ret@(sigma,(l,r)) <- oneb' p
              notElemWith sigma x (fv l)
              return ret
         return (sigma,(l, y.\Nu (x.\p')))
  <|> do (x,p) <- unbind b
         (sigma,(Up vy _,p')) <- local (extendCtx (Nab x)) $
          do ret@(sigma,(Up (Var y) (Var x'),_)) <- one p
             equalWith sigma x x' -- guard x==x' under substitution sigma
             noteqWith sigma x y  -- guard x/=y  under substitution sigma
             return ret
         return (sigma,(UpB vy, x.\p')) -- open
oneb _ = empty

oneb' p =
  do (sigma,(l,b)) <- oneb p
     r <- unbind b
     return (sigma,(l,r))
