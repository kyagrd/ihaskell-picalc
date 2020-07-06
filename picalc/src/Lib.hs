module Lib
    ( someFunc
    , fv
    , trace
    ) where

import Control.Lens.Fold
import Data.List
import Data.Typeable
import Unbound.Generics.LocallyNameless hiding (fv)
import qualified Unbound.Generics.LocallyNameless as U
import qualified Debug.Trace as Trace

-- trace = flip const
trace = Trace.trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fv :: (Alpha a, Typeable b) => a -> [Name b]
fv = sort . nub . toListOf U.fv
