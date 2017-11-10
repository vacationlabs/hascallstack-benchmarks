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

catchExceptions :: (Integer -> Integer) -> Integer -> IO ()
catchExceptions fn n = catch runFn $ \ ((!e) :: CustomException) -> e `deepseq` pure ()
  where
    runFn = do
      let x = fn n
      traceIO (show x)



main :: IO ()
main = defaultMain
  [
    bgroup "factorial"
    [ bench "regular/20" $ nfIO $ catchExceptions factorial 20
    , bench "callstack/20" $ nfIO $ catchExceptions factorialCS 20
    , bench "regular/30" $ nfIO $ catchExceptions factorial 30
    , bench "callstack/30" $ nfIO $ catchExceptions factorialCS 30
    , bench "regular/50" $ nfIO $ catchExceptions factorial 50
    , bench "callstack/50" $ nfIO $ catchExceptions factorialCS 50
    , bench "regular/100" $ nfIO $ catchExceptions factorial 100
    , bench "callstack/100" $ nfIO $ catchExceptions factorialCS 100
    , bench "regular/200" $ nfIO $ catchExceptions factorial 200
    , bench "callstack/200" $ nfIO $ catchExceptions factorialCS 200
    , bench "regular/300" $ nfIO $ catchExceptions factorial 300
    , bench "callstack/300" $ nfIO $ catchExceptions factorialCS 300
    ]
  ]

