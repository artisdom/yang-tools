{-# LANGUAGE CPP, OverloadedStrings #-}

module Data.Yang.Module.Parser.Internal
    (
      yModule
    , statement
    , keyword
    , identifier
    , argument
    ) where

import Data.ByteString.Builder
    (Builder, byteString, toLazyByteString, word8)
import Control.Applicative ((*>), (<|>), (<$>))
import Data.Attoparsec.ByteString.Char8 (Parser, skipSpace, skipMany)
import Data.ByteString.Char8 (ByteString, cons)
import Data.Attoparsec.Combinator ((<?>))
import Data.Monoid (mappend, mempty)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Data.Yang.Module.Types (Keyword(..), Statement(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as L
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Attoparsec.Zepto as Z

#define ASTERISK 42
#define BACKSLASH 92
#define CARRIAGE_RETURN 13
#define CLOSE_CURLY 125
#define COLON 58
#define DASH 45
#define DOT 46
#define DOUBLE_QUOTE 34
#define LINE_FEED 10
#define OPEN_CURLY 123
#define PLUS 43
#define SEMICOLON 59
#define SINGLE_QUOTE 39
#define SLASH 47
#define SPACE 32
#define TAB 9

yModule :: Parser Statement
yModule = do
    optSep
    s <- statement
    optSep
    A.endOfInput
    if kw s == BuiltIn "module" || kw s == BuiltIn "submodule"
        then return s 
        else fail "missing '(sub)module' keyword"

statement :: Parser Statement
statement = do
    k <- keyword
    a <- argument
    optSep
    next <- A.anyWord8
    ss <- case next of
              SEMICOLON  -> return []
              OPEN_CURLY -> substatements
              _          -> fail ("statement not properly terminated " ++ show next)
    return $ Statement k a ss

keyword :: Parser Keyword 
keyword = do
    x <- identifier
    next <- A.peekWord8'
    if next == COLON
        then do
            A.word8 COLON
            y <- identifier
            return $ Extension x y
        else
            return $ BuiltIn x

identifier :: Parser ByteString
identifier = do
    first <- C.satisfy (C.inClass "a-zA-Z_") <?> "identifier"
    rest <- C.takeWhile (C.inClass "a-zA-Z0-9._-") <?> "identifier"
    return $ cons first rest

substatements :: Parser [Statement]
substatements = do
    optSep
    next <- A.peekWord8'
    if next == CLOSE_CURLY
        then A.anyWord8 >> return []
        else do
            hd <- statement
            tl <- substatements
            return (hd : tl)

argument :: Parser Text
argument = (sep >> argument') <|> return ""

argument' :: Parser Text
argument' = do
    next <- A.peekWord8'
    case next of
        SEMICOLON    -> return ""
        OPEN_CURLY   -> return ""
        SINGLE_QUOTE -> qString
        DOUBLE_QUOTE -> qString
        _            -> uString

uString :: Parser Text
uString = do
    raw <- A.scan SPACE uArg
    bs <- ccom raw
    case decodeUtf8' bs of
        Right res -> return res
        Left err    -> fail $ show err
  where
    uArg SEMICOLON _ = Nothing
    uArg SLASH SLASH = Just SEMICOLON
    uArg SLASH ASTERISK = Just SEMICOLON
    uArg ASTERISK SLASH = Just SEMICOLON
    uArg _ c
        | c `B.elem` " \n\r\t{};" = Nothing
        | otherwise = Just c
    take2 bs = B.take (B.length bs - 2) bs
    ccom str
        | "//" `B.isSuffixOf` str =
            eatLineComment >> return (take2 str)
        | "/*" `B.isSuffixOf` str =
            eatBlockComment >> return (take2 str)
        | "*/" `B.isSuffixOf` str = fail "misplaced comment terminator"
        | otherwise = return str

utf8decode :: ByteString -> Parser Text
utf8decode bs = case decodeUtf8' bs of
    Right res -> return res
    Left err  -> fail $ show err

qString :: Parser Text
qString = do
    ss <- (sqString <|> dqString) `A.sepBy` (optSep >> A.word8 PLUS >> optSep)
    utf8decode $ B.concat ss

sqString :: Parser B.ByteString
sqString = do
    A.word8 SINGLE_QUOTE
    s <- A.takeWhile (/= SINGLE_QUOTE)
    A.word8 SINGLE_QUOTE
    return s

dqString :: Parser B.ByteString
dqString = A.word8 DOUBLE_QUOTE *> dqString'

dqString' :: Parser B.ByteString
dqString' = do
    s <- A.scan False $
        \s c -> if s then Just False
                     else if c == DOUBLE_QUOTE
                              then Nothing
                              else Just (c == BACKSLASH)
    A.word8 DOUBLE_QUOTE
    if BACKSLASH `B.elem` s
        then case Z.parse unescape s of
            Right res -> return res
            Left err -> fail err
        else return s

unescape :: Z.Parser B.ByteString
unescape = toByteString <$> go mempty
  where
    go acc = do
        h <- Z.takeWhile (/= BACKSLASH)
        let
          rest = do
            eseq <- Z.take 2
            let next w = go $ acc `mappend` byteString h `mappend` word8 w
            case B.last eseq of
                DOUBLE_QUOTE -> next DOUBLE_QUOTE
                BACKSLASH    -> next BACKSLASH
                110          -> next LINE_FEED
                116          -> next TAB
                _            -> fail "invalid escape sequence"
        done <- Z.atEnd
        if done
            then return (acc `mappend` byteString h)
            else rest

eatLineComment :: Parser ()
eatLineComment = A.takeTill (== LINE_FEED) >> return ()

eatBlockComment :: Parser ()
eatBlockComment = do
    A.scan False $ \s c ->
        if s && (c == SLASH)
            then Nothing
            else Just (c == ASTERISK)
    A.anyWord8
    return ()

lineComment :: Parser ()
lineComment = A.string "//" >> eatLineComment

blockComment :: Parser ()
blockComment = A.string "/*" >> eatBlockComment

yComment :: Parser ()
yComment = lineComment <|> blockComment

optSep :: Parser ()
optSep = A.skipMany $ yComment <|> A.skip wsp

sep :: Parser ()
sep = A.skipMany1 $ yComment <|> A.skip wsp

wsp :: Word8 -> Bool
wsp w = w == SPACE || w == TAB || w == LINE_FEED

prtext (Right x) = T.putStrLn x
prtext (Left x) = print x

toByteString :: Builder -> ByteString
toByteString = L.toStrict . toLazyByteString
