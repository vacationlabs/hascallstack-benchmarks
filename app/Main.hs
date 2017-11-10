{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

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
import System.Random

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

recur :: Int -> Bool -> IO [UTCTime]
recur n b = case (b, n) of
  (True, 3) -> throw $ CustomException "Error from non-CS version" emptyCallStack
  (_, 1) -> getCurrentTime >>= (\t -> pure [t])
  _ -> do
    t <- getCurrentTime
    x <- recur (n - 1) b
    pure (t:x)

shouldThrowError :: IO Bool
shouldThrowError = do
  r :: Int <- randomRIO (0, 100)
  pure (r < 20)

recurCS :: (HasCallStack) => Int -> Bool -> IO [UTCTime]
recurCS n b = case (b, n) of
  (True, 3) -> throw $ CustomException "Error from CS version" callStack
  (_, 1) -> getCurrentTime >>= (\t -> pure [t])
  _ -> do
    t <- getCurrentTime
    x <- recurCS (n - 1) b
    pure (t:x)

spuriousSrcLoc :: SrcLoc
spuriousSrcLoc = SrcLoc{ srcLocPackage="hascallstack-benchmarks"
                       , srcLocModule="Main"
                       , srcLocFile="Main.hs"
                       , srcLocStartLine=0
                       , srcLocStartCol=0
                       , srcLocEndLine=0
                       , srcLocEndCol=0}

recurCSExplicit :: MyCallStack -> Int -> Bool -> IO [UTCTime]
recurCSExplicit cs n b = case (b, n) of
  (True, 3) -> throw $ CustomException2 "Error from CS version" (PushCallStack "recurCSExplicit" spuriousSrcLoc cs)
  (_, 1) -> getCurrentTime >>= (\t -> pure [t])
  _ -> do
    t <- getCurrentTime
    x <- recurCSExplicit (PushCallStack "recurCSExplicit" spuriousSrcLoc cs) (n - 1) b
    pure (t:x)

recurCSExplicitMini :: CallStackMini -> Int -> Bool -> IO [UTCTime]
recurCSExplicitMini cs n b = case (b, n) of
  (True, 3) -> throw $ CustomException3 "Error from CS version" (PushCallStackMini () () cs)
  (_, 1) -> getCurrentTime >>= (\t -> pure [t])
  _ -> do
    t <- getCurrentTime
    x <- recurCSExplicitMini (PushCallStackMini () () cs) (n - 1) b
    pure (t:x)


catchExceptions :: (Int -> Bool -> IO [UTCTime]) -> Int -> IO ()
catchExceptions fn n = catches runFn [ Handler $ \ ((!e) :: CustomException) -> e `deepseq` pure ()
                                     , Handler $ \ ((!e) :: CustomException2) -> e `deepseq` pure ()
                                     , Handler $ \ ((!e) :: CustomException3) -> e `deepseq` pure ()
                                     ]
  where
    runFn = do
      s <- shouldThrowError
      x <- fn n s
      x `deepseq` pure ()

main :: IO ()
main = do
  let myCallStack = EmptyCallStack
      callStackMini = EmptyCallStackMini
  defaultMain
    [
      bgroup "recur" $
      Prelude.concatMap
      (\x -> [ bench ("no callstack/" ++ (show x)) $ nfIO $ catchExceptions recur x
             , bench ("HasCallStack/" ++ (show x)) $ nfIO $ catchExceptions recurCS x
             , bench ("explicit callstack/" ++ (show x)) $ nfIO $ catchExceptions (recurCSExplicit myCallStack) x
             , bench ("explicit callstack (mini)/" ++ (show x)) $ nfIO $ catchExceptions (recurCSExplicitMini callStackMini) x
             ])
      [1000, 10000, 100000]
    -- [ bench "regular/100" $ nfIO $ catchExceptions recur 100
    -- , bench "callstack/100" $ nfIO $ catchExceptions recurCS 100
    -- , bench "regular/200" $ nfIO $ catchExceptions recur 200
    -- , bench "callstack/200" $ nfIO $ catchExceptions recurCS 200
    -- , bench "regular/300" $ nfIO $ catchExceptions recur 300
    -- , bench "callstack/300" $ nfIO $ catchExceptions recurCS 300
    -- ]
    ]

