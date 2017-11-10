{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Text as T
import GHC.Stack
import Debug.Trace
import Control.Exception
import Control.DeepSeq

-- TODO: Is this the correct way to implement an NFData for SomeException?
instance NFData SomeException where
  rnf e = rnf (show e)

factorialCS :: (HasCallStack) => Integer -> Integer
factorialCS 1 = 1
factorialCS 3 = error "Error from CS version"
factorialCS n = n * factorialCS (n - 1)

factorial :: Integer -> Integer
factorial 1 = 1
factorial 3 = error "Error from non-CS version"
factorial n = n * factorialCS (n - 1)

catchExceptions :: (Integer -> Integer) -> IO ()
catchExceptions fn = catch runFn $ \ ((!e) :: SomeException) -> e `deepseq` pure ()
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

