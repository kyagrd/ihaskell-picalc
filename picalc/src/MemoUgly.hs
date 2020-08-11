{-# LANGUAGE NoMonomorphismRestriction #-}
module MemoUgly where
import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.MemoUgly

memo = Data.MemoUgly.memo
memoIO = Data.MemoUgly.memoIO

memoFix ff = f where f = memo (ff f)