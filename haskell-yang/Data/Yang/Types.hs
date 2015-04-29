module Data.Yang.Types
    (
      -- * Core YANG types
      YangKeyword
    , YangStatement(..)
    , YangModule(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

type YangKeyword = (Maybe ByteString, ByteString)

data YangStatement = YSt
    { keyword  :: !YangKeyword
    , arg      :: !Text
    , substmts :: [YangStatement]
    } deriving (Show)

data YangModule = YMod
    { name       :: !ByteString
    , main       :: YangStatement
    , submodules :: [YangStatement]
    , features   :: [ByteString]
    } deriving (Show)

