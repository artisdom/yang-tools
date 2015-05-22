{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Attoparsec.ByteString
import System.Environment
import Data.Yang.Module
import qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    let fn = head args
    bs <- B.readFile fn
    let
      m = case parseOnly yModule bs of
        Left _ -> error "broken module"
        Right s -> s
    print $ findAll (BuiltIn "container") m
