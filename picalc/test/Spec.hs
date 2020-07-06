{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Lens.Fold
import Control.Monad.Trans.Reader
import Data.Typeable
import Unbound.Generics.LocallyNameless hiding (fv)

import Lib
import PiCalc
import OpenLTS
import OpenBisim

alphas = map (:[]) ['a'..'y']

numVars = 4

newNm = s2n "z"

instance Arbitrary Nm where
  arbitrary = elements [s2n s | s<-take numVars alphas]

instance Arbitrary Tm where
  arbitrary = Var <$> arbitrary

instance Arbitrary PrB where
  arbitrary = freqPrB 1

instance Arbitrary Pr where
  arbitrary = freqPr 1

freqPrB n = do
    p <- arbPr 
    x <- elements (newNm : fv p)
    return $ bind x p
  where
    arbPr = freqPr (n+1)

freqPr n = frequency
    [ (n*n*n, pure Null)
    , (n*n, TauP <$> arbPr)
    , (n*n, Out <$> arbTm <*> arbTm <*> arbPr)
    , (n*n, In <$> arbTm <*> arbPrB)
    , (n, Match <$> arbTm <*> arbitrary <*> arbPr)
    , (n, Plus <$> arbPr <*> arbPr)
    , (1, Par <$> arbPr <*> arbPr)
    , (1, Nu <$> arbPrB)
    ]
  where
    arbTm = arbitrary 
    arbPr  = freqPr  (n+1)
    arbPrB = freqPrB (n+1)

main :: IO ()
main = hspec $ do
  bisimSpecRefl
  bisimSpecPlusSymm
  bisimSpecParSymm

bisimSpecRefl = describe "bisim reflexive" $ do 
    it (show p001) $ run_bisim_refl p001 `shouldBe` True
    it (show p002) $ run_bisim_refl p002 `shouldBe` True
    it (show p003) $ run_bisim_refl p003 `shouldBe` True
    it "arbitrary same processes" $ 
      withMaxSuccess 5000 . property $ \p -> run_bisim_refl (trace (show p) p) `shouldBe` True

bisimSpecPlusSymm = describe "bisim Plus symm" $ -- do 
    it "arbitrary plus processes" $ 
      withMaxSuccess 5000 . property $ \p q -> run_bisim (Plus p q) (Plus q p) `shouldBe` True

bisimSpecParSymm = describe "bisim Par symm" $ -- do 
    it "arbitrary par processes" $ 
      withMaxSuccess 100 . property $ \p q -> run_bisim (Par p q) (Par q p) `shouldBe` True

run_bisim_refl p = run_bisim p p

[a,b,c,d,e] = s2n <$> take 5 alphas :: [Nm]

p001 = Match (Var c) (Var b) (Nu (c.\ Out (Var b) (Var b) Null))
p002 = Match (Var c) (Var b) (Nu (c.\ In (Var b) (e.\ Null)))
p003 = Match (Var d) (Var c) (Nu (d.\ Match (Var d) (Var c) (TauP Null)))
