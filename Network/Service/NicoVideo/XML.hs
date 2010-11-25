{- -}
{-
- nicotool
- Copyright (C) 2020, HATTORI, Hiroki
- XMLパース用ヘルパ
-
- Released under FreeBSD Licence.
-}
module Network.Service.NicoVideo.XML
       (TagParserT, RequestError (..),
        parseTag,
        tagOpen, tagClose,
        element, readElement, boolElement, unixTimeElement, txt, txt'
       ) where

import qualified Text.HTML.TagSoup as TS
import Text.Parsec
import Text.StringLike
import System.Time


type TagParserT str u m = ParsecT [TS.Tag str] u m
data RequestError = RequestError String String deriving(Show)


--
-- XML パース用ヘルパ
--

tagEater :: (Show str, StringLike str, Monad m) => (TS.Tag str -> Maybe a) -> TagParserT str u m a
tagEater = tokenPrim show (\x _ _ -> setSourceLine x (sourceLine x + 1))

tagOpen :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m (TS.Tag str)
tagOpen name = tagEater $ (\x -> if TS.isTagOpenName name x then Just x else Nothing)

tagClose :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m ()
tagClose name = tagEater $ (\x -> if TS.isTagCloseName name x then Just () else Nothing)

txt :: (Show str, StringLike str, Monad m) => TagParserT str u m str
txt = do { tagEater (\x -> if TS.isTagText x then Just (TS.fromTagText x) else Nothing); } <|> return (Text.StringLike.fromString "")

txt' :: (Show str, StringLike str, Monad m) => a -> TagParserT str u m str
txt' _ = txt


element :: (Show str, StringLike str, Monad m) => str -> (TS.Tag str -> TagParserT str u m a) -> TagParserT str u m a
element name bdy = do { ot <- Text.Parsec.try (skipText >> tagOpen name); x <- bdy ot; skipText; tagClose name; return x }
  where skipText = skipMany $ tagEater (\x -> if TS.isTagText x then Just x else Nothing)

parseTag :: (Show str, StringLike str, Monad m) => TagParserT str u m a -> u -> SourceName -> [TS.Tag str] -> m (Either RequestError a)
parseTag p st name doc = runPT p st name doc >>= return . (either (Left . RequestError "Parser error" . show ) Right)


readElement :: (Read b, Show str, StringLike str, Monad m) => str -> TagParserT str u m b
readElement name = element name (\_ -> txt >>= return . read . Text.StringLike.toString)

unixTimeElement :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m ClockTime
unixTimeElement name = return . flip TOD 0 =<< readElement name

boolElement :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m Bool
boolElement name = do { x <- element name txt'; return $ Text.StringLike.toString x == "1"; }

