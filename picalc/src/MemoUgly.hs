{-# LANGUAGE NoMonomorphismRestriction #-}
module MemoUgly where
import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.MemoUgly

umemo = Data.MemoUgly.memo
umemoIO = Data.MemoUgly.memoIO

umemoFix ff = f where f = umemo (ff f)