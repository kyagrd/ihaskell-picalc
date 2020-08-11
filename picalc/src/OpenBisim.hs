{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module OpenBisim where

import Control.Applicative
import Control.Lens.Fold
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Reader
import qualified Control.Monad.Fail as Fail
import Data.List
import Data.Maybe
import Data.Tree
import Data.Partition hiding (empty,rep)
import qualified Data.Partition as P
import qualified Data.Set as Set
import Unbound.Generics.LocallyNameless hiding (fv)
import MemoUgly

import Lib
import PiCalc
import qualified IdSubLTS as IdS
import OpenLTS

-- instance MonadCache (Pr,Pr) Bool (FreshMT []) where
--   lookup k = lift $ MemoClass.lookup k
--   add k v  = lift $ MemoClass.add k v

data StepLog = One  Ctx EqC Act  Pr
             | OneB Ctx EqC ActB PrB
             deriving (Eq,Ord,Show)

type EqC = [(Nm,Nm)]

toEqC ctx sigma = [(x',x) | x:xs <- eqcs, x'<-xs]
  where
    ns = quan2nm <$> reverse ctx
    eqcs = map (ns!!) <$> (Set.toList <$> P.nontrivialSets sigma)

stepLog  eitherC ctx sigma l p = Node . eitherC $ One  ctx (toEqC ctx sigma) l p
stepLogB eitherC ctx sigma l b = Node . eitherC $ OneB ctx (toEqC ctx sigma) l b

applySubst m = do (sigma,r) <- m
                  ctx <- ask 
                  return (sigma, subs ctx sigma r)

fvMaxInteger p = maximum $ 0 : map name2Integer (fv p :: [Nm])

runSim ctx p q = and $ run sim ctx p q
runBisim ctx p q = and $ run bisim ctx p q

run_sim p q = and $ run_ sim p q
run_bisim p q = and $ run_ bisim p q

runSim' = run sim'
runBisim' = run bisim'

run_sim' = run_ sim'
run_bisim' = run_ bisim'

run f ctx p q = f p q `runReaderT` ctx `contFreshMT` (1+fvMaxInteger ctx) 

run_ f p q = run f (All<$>fv(p,q)) p q

sim = simBool_ id sim
sim' = simStepLog_ Left Right id sim'

{-
-- bisimMemo = curry $ memoFix bisim_unfix

bisim_unfix f (p,q)
  | p `aeq` q = return True -- to test bisim refl, comment this line
  | otherwise = simBool_ id   (curry f) p q
            <|> simBool_ flip (curry f) q p

-- bisim = bisimMemo
-}

-- bisimM (p,q)
--   | p `aeq` q = return True -- to test bisim refl, comment this line
--   | otherwise = simBool_ id   (curry $ memo bisimM) p q
--             <|> simBool_ flip (curry $ memo bisimM) q p

-- cmemo = curry . memo . uncurry

bisim p q
  | p `aeq` q = return True -- to test bisim refl, comment this line
  | otherwise = simBool_ id   bisim p q
            <|> simBool_ flip bisim q p

bisim' p q = simStepLog_ Left  Right id   bisim' p q
         <|> simStepLog_ Right Left  flip bisim' q p

simBool_ = sim_ or' and' or' and'
  where
    or'  _ _ _ _ = or  :: [Bool] -> Bool
    and' _ _ _ _ = and :: [Bool] -> Bool

simStepLog_ eitherP eitherQ =
    sim_ (stepLog  eitherP) (stepLog  eitherQ)
         (stepLogB eitherP) (stepLogB eitherQ)

sim_ logLeader  logFollow
     logLeaderB logFollowB
     h   -- either id or flip
     rf  -- recursive function call
     p q = 
      do (sigma,(lp,p')) <- applySubst $ sone p
         ctx <- ask
         return . logLeader ctx sigma lp p'
                . (`contFreshMT` (1+fvMaxInteger ctx))
                $ do (lq,q') <- IdS.sone (subs ctx sigma q)
                     guard $ lp == lq
                     return . logFollow ctx sigma lq q'
                            . (`contFreshMT` (1+fvMaxInteger ctx))
                            . (`runReaderT` ctx)
                            $ h rf p' q'
  <|> do (sigma,(lp,bp')) <- applySubst $ soneb p
         ctx <- ask
         (x',p') <- unbind bp'
         return . logLeaderB ctx sigma lp bp'
                . (`contFreshMT` (1+fvMaxInteger (x',ctx)))
                $ do (lq,bq') <- IdS.soneb (subs ctx sigma q)
                     guard $ lp == lq
                     (x,_,q1) <- unbind2' bp' bq'; let q' = subst x (Var x') q1
                     let ctx' = case lp of { DnB _ -> All x'; UpB _ -> Nab x' } : ctx
                     return . logFollowB ctx sigma lq bq'
                            . (`contFreshMT` (1+fvMaxInteger ctx'))
                            . (`runReaderT` ctx')
                            $ h rf p' q'


forest2df :: [Tree (Either StepLog StepLog)] -> [(Form,Form)]
forest2df rs
            =    do  Node (Left (One _ sigma_p a _)) [] <- rs
                     let sigmaqs = subsMatchingAct a (right1s rs)
                     return (prebase sigma_p a, postbase sigmaqs a)
            <|>  do  Node (Right (One _ sigma_q a _)) [] <- rs
                     let formR = prebase sigma_q a
                     let sigmaps = subsMatchingAct a (left1s rs)
                     return (postbase sigmaps a, formR)
            <|>  do  Node (Left (OneB _ sigma_p a _)) [] <- rs
                     let sigmaqs = subsMatchingActB a (right1Bs rs)
                     return (preBbase sigma_p a, postBbase sigmaqs a)
            <|>  do  Node (Right (OneB _ sigma_q a _)) [] <- rs
                     let formR = preBbase sigma_q a
                     let sigmaps = subsMatchingActB a (left1Bs rs)
                     return (postBbase sigmaps a, formR)
            <|>  do  Node (Left (One _ sigma_p a _)) rsR <- rs
                     let rss' = [rs' | Node _ rs' <- rsR]
                     (dfsL,dfsR) <- unzip <$> sequence (forest2df <$> rss')
                     guard . not . null $ dfsL
                     let sigmaqs = subsMatchingAct a (right1s rs)
                     return (pre sigma_p a dfsL, post sigmaqs a dfsR)
            <|>  do  Node (Right (One _ sigma_q a _)) rsL <- rs
                     let rss' = [rs' | Node _ rs' <- rsL]
                     (dfsL,dfsR) <- unzip <$> sequence (forest2df <$> rss')
                     guard . not . null $ dfsL
                     let sigmaps = subsMatchingAct a (left1s rs)
                     return (post sigmaps a dfsL, pre sigma_q a dfsR)
            <|>  do  Node (Left (OneB nctx sigma_p a _)) rsR <- rs
                     let  rss' = [rs' | Node _ rs' <- rsR]
                          x = quan2nm . head . getCtx . fromEither
                            . rootLabel . head $ head rss'
                     (dfsL,dfsR) <- unzip <$> sequence (forest2df <$> rss')
                     guard . not . null $ dfsL
                     let sigmaqs = subsMatchingActB a (right1Bs rs)
                     return (preB sigma_p a x dfsL, postB sigmaqs a x dfsR)
            <|>  do  Node (Right (OneB nctx sigma_q a _)) rsL <- rs
                     let  rss' = [rs' | Node _ rs' <- rsL]
                          x = quan2nm . head . getCtx . fromEither . rootLabel
                                $ head (head rss')
                     (dfsL,dfsR) <- unzip <$> sequence (forest2df <$> rss')
                     guard . not . null $ dfsL
                     let sigmaps = subsMatchingActB a (left1Bs rs)
                     return (postB sigmaps a x dfsL, preB sigma_q a x dfsR)
  where
    prebase sigma a = pre sigma a []
    postbase sigmas a = post sigmas a []
    preBbase sigma a = preB sigma a (s2n "?") []
    postBbase sigmas a = postB sigmas a (s2n "?") []
    pre sigma a = boxMat sigma . Dia a . conj
    post sigmas a fs = Box a . disj $  (diaMat<$>sigmas) ++ fs
    preB sigma a x = boxMat sigma . DiaB a . bind x . conj
    postB sigmas a x fs = BoxB a . bind x . disj $  (diaMat<$>sigmas) ++ fs
    boxMat  [] = id; boxMat  sigma = BoxMatch [(Var x,Var y) | (x,y)<-sigma]
    diaMat  [] = FF; diaMat  sigma = DiaMatch [(Var x,Var y) | (x,y)<-sigma]
    right1s  rs = [log | Node (Right  log@One{}) _ <- rs]
    left1s   rs = [log | Node (Left   log@One{}) _ <- rs]
    right1Bs  rs = [log | Node (Right  log@OneB{}) _ <- rs]
    left1Bs   rs = [log | Node (Left   log@OneB{}) _ <- rs]
    getCtx (One   nctx _ _ _)  = nctx; getCtx (OneB  nctx _ _ _) = nctx
    fromEither (Left   t) = t; fromEither (Right  t) = t

subsMatchingAct :: Act -> [StepLog] -> [EqC]
subsMatchingAct a logs =
  do  One ctx sigma' a' _ <-logs          ;  let sigmaSubs = subs' ctx sigma'
      guard $ sigmaSubs a == sigmaSubs a' ;  return sigma'

subsMatchingActB :: ActB -> [StepLog] -> [EqC]
subsMatchingActB a logs =
  do  OneB ctx sigma' a' _ <-logs         ;  let sigmaSubs = subs' ctx sigma'
      guard $ sigmaSubs a == sigmaSubs a' ;  return sigma'

subs' ctx eqc = substs [(x,Var y) | (x,y) <- eqc]