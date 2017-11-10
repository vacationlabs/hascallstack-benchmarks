{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Criterion.Main
import Data.Text as T
import GHC.Stack
import Debug.Trace
import Control.Exception
import Control.DeepSeq
import Data.Time
import GHC.Generics
import Prelude

data MyCallStack
  = EmptyCallStack
  | PushCallStack [Char] SrcLoc MyCallStack
  | FreezeCallStack MyCallStack deriving (Show, Generic)

data CallStackMini
  = EmptyCallStackMini
  | PushCallStackMini () () CallStackMini
  | CallStackMini CallStackMini deriving (Show, Generic)


data CustomException = CustomException String CallStack deriving (Show, Generic)
instance Exception CustomException
instance NFData CustomException

data CustomException2 = CustomException2 String MyCallStack deriving (Show, Generic)
instance Exception CustomException2
instance NFData MyCallStack
instance NFData CustomException2

data CustomException3 = CustomException3 String CallStackMini deriving (Show, Generic)
instance Exception CustomException3
instance NFData CallStackMini
instance NFData CustomException3

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

recur :: Int -> IO [UTCTime]
recur 1 = do
  t <- getCurrentTime
  pure [t]
recur 3 = throw $ CustomException "Error from non-CS version" emptyCallStack
recur n = do
  t <- getCurrentTime
  x <- recur (n - 1)
  pure (t:x)

recurCS :: (HasCallStack) => Int -> IO [UTCTime]
recurCS 1 = do
  t <- getCurrentTime
  pure [t]
recurCS 3 = throw $ CustomException "Error from CS version" callStack
recurCS n = do
  t <- getCurrentTime
  x <- recurCS (n - 1)
  pure (t:x)

spuriousSrcLoc :: SrcLoc
spuriousSrcLoc = SrcLoc{ srcLocPackage="hascallstack-benchmarks"
                       , srcLocModule="Main"
                       , srcLocFile="Main.hs"
                       , srcLocStartLine=0
                       , srcLocStartCol=0
                       , srcLocEndLine=0
                       , srcLocEndCol=0}

recurCSExplicit :: MyCallStack -> Int -> IO [UTCTime]
recurCSExplicit cs 1 = do
  t <- getCurrentTime
  pure [t]
recurCSExplicit cs 3 = do
  throw $ CustomException2 "Error from CS version" (PushCallStack "recurCSExplicit" spuriousSrcLoc cs)
recurCSExplicit cs n = do
  t <- getCurrentTime
  x <- recurCSExplicit (PushCallStack "recurCSExplicit" spuriousSrcLoc cs) (n - 1)
  pure (t:x)

recurCSExplicitMini :: CallStackMini -> Int -> IO [UTCTime]
recurCSExplicitMini cs 1 = do
  t <- getCurrentTime
  pure [t]
recurCSExplicitMini cs 3 = do
  throw $ CustomException3 "Error from CS version" (PushCallStackMini () () cs)
recurCSExplicitMini cs n = do
  t <- getCurrentTime
  x <- recurCSExplicitMini (PushCallStackMini () () cs) (n - 1)
  pure (t:x)


catchExceptions :: (Int -> IO [UTCTime]) -> Int -> IO ()
catchExceptions fn n = catches runFn [ Handler $ \ ((!e) :: CustomException) -> e `deepseq` pure ()
                                     , Handler $ \ ((!e) :: CustomException2) -> e `deepseq` pure ()
                                     , Handler $ \ ((!e) :: CustomException3) -> e `deepseq` pure ()
                                     ]
  where
    runFn = do
      x <- fn n
      traceIO (show x)

main :: IO ()
main = do
  let myCallStack = EmptyCallStack
      callStackMini = EmptyCallStackMini
  defaultMain
    [
      bgroup "recur" $
      Prelude.concatMap
      (\x -> [ bench ("regular/" ++ (show x)) $ nfIO $ catchExceptions recur x
             , bench ("HasCallStack/" ++ (show x)) $ nfIO $ catchExceptions recurCS x
             , bench ("explicit/" ++ (show x)) $ nfIO $ catchExceptions (recurCSExplicit myCallStack) x
             , bench ("explicitMini/" ++ (show x)) $ nfIO $ catchExceptions (recurCSExplicitMini callStackMini) x
             ])
      [10, 100]
    -- [ bench "regular/100" $ nfIO $ catchExceptions recur 100
    -- , bench "callstack/100" $ nfIO $ catchExceptions recurCS 100
    -- , bench "regular/200" $ nfIO $ catchExceptions recur 200
    -- , bench "callstack/200" $ nfIO $ catchExceptions recurCS 200
    -- , bench "regular/300" $ nfIO $ catchExceptions recur 300
    -- , bench "callstack/300" $ nfIO $ catchExceptions recurCS 300
    -- ]
    ]

