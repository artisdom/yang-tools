{-# LANGUAGE CPP, OverloadedStrings, BangPatterns #-}

module Data.Yang.Module.Parser
    (
      yStatement
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
import Data.Yang.Module.Types (QualIdent, DataType(..), YangStatement(..))
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
#define SEMICOLON 59
#define SINGLE_QUOTE 39
#define SLASH 47
#define SPACE 32
#define TAB 9

toText :: String -> Parser ByteString -> Parser Text
toText name p = (decodeUtf8 <$> p) <?> name

identifier :: Parser Text
identifier = do
    first <- C.satisfy (C.inClass "a-zA-Z_") <?> "identifier"
    rest <- C.takeWhile (C.inClass "a-zA-Z0-9._-") <?> "identifier"
    let id = decodeUtf8 (cons first rest)
    if T.length id < 3 || (T.toUpper . T.take 3) id /= "XML"
        then return id
        else fail "cannot start with XML (any case)." <?> "identifier"

trueFalse :: Parser Text
trueFalse = toText "trueFalse" $ C.string "true" <|> C.string "false"

yangVersion :: Parser Text
yangVersion = do
    major <- C.digit <?> "major version"
    A.word8 DOT <?> "decimal dot"
    minor <- C.digit <?> "minor version"
    return $ T.pack [major, '.', minor]

qualIdent :: Parser Text 
qualIdent = do
    x <- identifier
    next <- A.peekWord8'
    if next == COLON
        then do
            A.word8 COLON
            y <- identifier
            return $ T.concat [x, ":", y]
        else
            return x

revDate :: Parser Text
revDate = do
    y <- C.count 4 C.digit
    A.word8 DASH
    m <- C.count 2 C.digit
    A.word8 DASH
    d <- C.count 2 C.digit
    let y' = read y :: Int
    let d' = read d :: Int
    if d' > 0 && y' >= 2000 && dateCheck y' m d'
        then return $ T.pack $ y ++ ('-' : m) ++ ('-' : d)
        else fail "wrong date" <?> "revDate"
      where
        dateCheck y' m d'
            | m `elem` ["01", "03", "05", "07", "08", "10", "12"] = d' <= 31
            | m `elem` ["04", "06", "09", "11"] = d' <= 30
            | m == "02" = d' <= 28 || d' == 29 &&
                (y' `mod` 4 == 0 && y' `mod` 100 /= 0 || y' `mod` 400 == 0)
            | otherwise = False

-- range :: Parser Text

yStatement :: Parser YangStatement
yStatement = do
    k <- qualIdent
    a <- yArgument
    optSep
    next <- A.anyWord8
    ss <- case next of
              SEMICOLON  -> return []
              OPEN_CURLY -> ySub
              _          -> fail ("statement not properly terminated " ++ show next)
    return $ YSt k a ss

ySub :: Parser [YangStatement]
ySub = do
    optSep
    next <- A.peekWord8'
    if next == CLOSE_CURLY
        then A.anyWord8 >> return []
        else do
            hd <- yStatement
            tl <- ySub
            return (hd : tl)

yArgument :: Parser Text
yArgument = (sep >> yArgument') <|> return ""

yArgument' :: Parser Text
yArgument' = do
    next <- A.peekWord8'
    case next of
        SEMICOLON    -> return ""
        OPEN_CURLY   -> return ""
        SINGLE_QUOTE -> qString
        DOUBLE_QUOTE -> dqString
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
    A.word8 SINGLE_QUOTE
    s <- A.takeWhile (/= SINGLE_QUOTE)
    A.word8 SINGLE_QUOTE
    utf8decode s

dqString :: Parser Text
dqString = A.word8 DOUBLE_QUOTE *> dqString'

dqString' :: Parser Text
dqString' = do
    s <- A.scan False $
        \s c -> if s then Just False
                     else if c == DOUBLE_QUOTE
                              then Nothing
                              else Just (c == BACKSLASH)
    A.word8 DOUBLE_QUOTE
    s1 <- if BACKSLASH `B.elem` s
          then case Z.parse unescape s of
              Right res -> return res
              Left err -> fail err
          else return s
    case decodeUtf8' s1 of
        Right res -> return res
        Left err -> fail $ show err

unescape :: Z.Parser ByteString
unescape = toByteString <$> go mempty where
    go acc = do
        h <- Z.takeWhile (/= BACKSLASH)
        let rest = do
                start <- Z.take 2
                let !slash = B.unsafeHead start
                    !t = B.unsafeIndex start 1
                    escape = case B.findIndex (== t) "\"\\nt" of
                        Just i -> i
                        _      -> 255
                if slash /= BACKSLASH || escape == 255
                    then fail "invalid escape sequence"
                    else go (acc `mappend` byteString h `mappend`
                        word8 (B.unsafeIndex "\"\\\n\t" escape))
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
