{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Data.Tree
import qualified IdSubLTS as IdS
-- import MemoUgly
import OpenLTS
import PiCalc
import Unbound.Generics.LocallyNameless hiding (fv)

instance MonadFail m => MonadFail (FreshMT m) where
  fail = runFreshMT . Fail.fail

sim :: Ctx -> Pr -> Pr -> [Bool]
sim ctx p q =
      do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ one p
         let sigmaSub = subs ctx sigma
         let (lp,p') = sigmaSub r
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lq,q') <- IdS.one (sigmaSub q)
           guard $ lp == lq
           return . (and :: [Bool] -> Bool) $ sim ctx p' q'
  <|> do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ oneb p
         let sigmaSub = subs ctx sigma
         let (lp,bp') = sigmaSub r
         let (x',p') = runFreshM $ do { mapM_ fresh (quan2nm <$> ctx); unbind bp' }
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lq,bq') <- IdS.oneb (sigmaSub q)
           guard $ lp == lq
           (x,q1) <- unbind bq'
           let q' = subst x (Var x') q1
           let ctx' = case lp of { DnB _ -> All x; UpB _ -> Nab x } : ctx
           return . (and :: [Bool] -> Bool) $ sim ctx' p' q'

bisim :: Ctx -> Pr -> Pr -> [Bool]
bisim ctx p q =
      do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ one p
         let sigmaSub = subs ctx sigma
         let (lp,p') = sigmaSub r
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lq,q') <- IdS.one (sigmaSub q)
           guard $ lp == lq
           return . (and :: [Bool] -> Bool) $ sim ctx p' q'
  <|> do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ oneb p
         let sigmaSub = subs ctx sigma
         let (lp,bp') = sigmaSub r
         let (x',p') = runFreshM $ do { mapM_ fresh (quan2nm <$> ctx); unbind bp' }
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lq,bq') <- IdS.oneb (sigmaSub q)
           guard $ lp == lq
           (x,q1) <- unbind bq'
           let q' = subst x (Var x') q1
           let ctx' = case lp of { DnB _ -> All x'; UpB _ -> Nab x' } : ctx
           return . (and :: [Bool] -> Bool) $ bisim ctx' p' q'
  <|> do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ one q
         let sigmaSub = subs ctx sigma
         let (lq,q') = sigmaSub r
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lp,p') <- IdS.one (sigmaSub p)
           guard $ lp == lq
           return . (and :: [Bool] -> Bool) $ bisim ctx p' q'
  <|> do (sigma,r) <- runFreshMT . (`runReaderT` ctx) $ oneb q
         let sigmaSub = subs ctx sigma
         let (lq,bq') = sigmaSub r
         let (x',q') = runFreshM $ do { mapM_ fresh (quan2nm <$> ctx); unbind bq' }
         return . (or :: [Bool] -> Bool) . runFreshMT $ do
           (lp,bp') <- IdS.oneb (sigmaSub p)
           guard $ lp == lq
           (x,p1) <- unbind bp';
           let p' = subst x (Var x') p1
           let ctx' = case lp of { DnB _ -> All x; UpB _ -> Nab x } : ctx
           return . (and :: [Bool] -> Bool) $ bisim ctx' p' q'


         
