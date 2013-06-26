{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Bencode
import           System.Exit (exitFailure)
import           Test.QuickCheck

main :: IO ()
main = putStrLn "this test always fails" >> exitFailure
