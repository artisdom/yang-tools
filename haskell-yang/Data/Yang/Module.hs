module Data.Yang.Module
    (
      Keyword(..)
    , Statement(..)
    , findOne
    , findAll
    , yModule
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Yang.Module.Parser (yModule)
import Data.Yang.Module.Types (Keyword(..), Statement(..))
import qualified Data.List as L

findOne :: Keyword -> Statement -> Maybe Statement
findOne k s = L.find (\x -> kw x == k) (substmts s)

findAll :: Keyword -> Statement -> [Statement]
findAll k s = L.filter (\x -> kw x == k) (substmts s)