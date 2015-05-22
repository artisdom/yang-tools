module Data.Yang.Module.Types
    (
      -- * Core YANG types
      Keyword(..)
    , Statement(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data Keyword
     = BuiltIn   !ByteString
     | Extension !ByteString !ByteString
     deriving (Eq, Show)

data Statement = Statement
    { kw       :: !Keyword
    , arg      :: !Text
    , substmts :: [Statement]
    } deriving (Show)
