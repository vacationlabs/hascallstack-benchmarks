{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Text as T
import GHC.Stack
import Debug.Trace
import Control.Exception
import Control.DeepSeq
import GHC.Generics

data CustomException = CustomException String CallStack deriving (Show, Generic)
instance Exception CustomException
instance NFData CustomException

-- TODO: Is this the correct way to implement an NFData for SomeException?
-- instance NFData SomeException where
--   rnf e = rnf (show e)

factorialCS :: (HasCallStack) => Integer -> Integer
factorialCS 1 = 1
factorialCS 3 = throw $ CustomException "Error from CS version" callStack
factorialCS n = n * factorialCS (n - 1)

factorial :: Integer -> Integer
factorial 1 = 1
factorial 3 = throw $ CustomException "Error from non-CS version" emptyCallStack
factorial n = n * factorial (n - 1)

catchExceptions :: (Integer -> Integer) -> IO ()
catchExceptions fn = catch runFn $ \ ((!e) :: CustomException) -> e `deepseq` pure ()
  where
    runFn = do
      let x = fn 20
      traceIO (show x)



main :: IO ()
main = defaultMain
  [
    bgroup "factorial"
    [ bench "regular" $ nfIO $ catchExceptions factorial
    , bench "callstack" $ nfIO $ catchExceptions factorialCS
    ]
  ]

