module Data.Yang.Module.Types
    (
      -- * Core YANG types
      Keyword(..)
    , Module(..)
    , Statement(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data Keyword
     = BuiltIn   !ByteString
     | Extension !ByteString !ByteString
     deriving (Eq, Show)

data Statement = Statement
    { kw    :: !Keyword
    , arg   :: !Text
    , stmts :: [Statement]
    } deriving (Show)

data Module
    = Module    !Text [Statement]
    | Submodule !Text [Statement]
    deriving (Show)
