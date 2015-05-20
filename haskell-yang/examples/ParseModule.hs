module Main (main) where

import Data.Attoparsec.ByteString.Lazy
import System.Environment
import Data.Yang.Module.Parser
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do args <- getArgs
          let fn = head args
          bs <- B.readFile fn
          parseTest yModule bs
