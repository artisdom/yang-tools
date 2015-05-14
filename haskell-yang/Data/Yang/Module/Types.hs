module Data.Yang.Module.Types
    (
      -- * Core YANG types
      DataType(..)
    , QualIdent
    , YangStatement(..)
    , YangModule(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Text (Text)

type Range a = [(Maybe a, Maybe a)]
type QualIdent = (Maybe ByteString, ByteString)

data YangStatement = YSt
    { keyword  :: !Text
    , arg      :: !Text
    , substmts :: [YangStatement]
    } deriving (Show)

data YangModule = YMod
    { name       :: !ByteString
    , main       :: YangStatement
    , submodules :: [YangStatement]
    , features   :: [ByteString]
    } deriving (Show)

data DataType
    = Yint8 { range8 :: Range Int8 }
    | Yint16 { range16 :: Range Int16 }
    deriving (Show)
