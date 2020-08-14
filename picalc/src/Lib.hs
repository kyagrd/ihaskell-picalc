module Lib
    ( someFunc
    , trace
    ) where

import qualified Debug.Trace as Trace

-- trace = flip const
trace = Trace.trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"
