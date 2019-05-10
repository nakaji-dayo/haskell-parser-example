{-# LANGUAGE OverloadedStrings #-}
module AttoparsecSample where

import           Control.Applicative
import           Data.Attoparsec.Text as P
-- import           Data.Attoparsec.Types
import           Data.Char
import           Data.Text            (Text (..))

test = do
  run date "date" "2019-04-21"
  run date "date" "hoge"
  run date' "date'" "2019-04-21"
  run date' "date'" "hoge"
  run url "url" "http://yahoo.co.jp/"
  run url "url" "hoge+zzz://fuga.com/x-y/z?a=b&c&d=8"
  let html = "<p><code>manyTill p end</code> applies parser <code>p</code> <em>zero</em> or more times until parser <code>end</code> succeeds. Returns the list of values returned by <code>p</code>. <a href=\"https://hackage.haskell.org/package/parsec-3.1.13.0\">ref</a> </p>"
  run ((extractInnerHtml "code")) "extractInnerHtml" html
  run (extractInnerHtml "code") "extractInnerHtml" "hogehoge"
  run ((extractInnerHtmlMany "code")) "extractInnerHtmlMany" html
  run (extractInnerHtmlMany "code") "extractInnerHtmlMany" "hogehoge"
  run sumNumber "sumNumber" "a:500円  b:600円、123円."
  let md = "# hoge\naiueo\n\n- a\n- b\n- c\n\n## fuga"
  run markdown "markdown" md

run :: Show a => Parser a -> String -> Text -> IO ()
run parser name input = do
  putStrLn $ concat ["----", name, "----"]
  print $ parseOnly parser input

date :: Parser (Int, Int, Int)
date = do
  y <- count 4 digit
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  return (read y, read m, read d)

date' :: Parser (Int, Int, Int)
date' = (,,) <$> (read <$> count 4 digit <* char '-')
  <*> (read <$> count 2 digit <* char '-')
  <*> (read <$> count 2 digit)

url :: Parser (String, String, [String], [(String, Maybe String)])
url = do
  s <- many (letter <|> choice (char <$> "+-"))
  string "://"
  d <- many urlSafeChar
  ps <- many $ char '/' *> many urlSafeChar
  qs <- option [] $ do
    char '?'
    pair `sepBy` char '&'
  return (s, d, ps, qs)

urlSafeChar :: Parser Char
urlSafeChar = letter <|> digit <|> choice (char <$> ".~_-")

pair :: Parser (String, Maybe String)
pair = (,) <$> many urlSafeChar <*> (option Nothing (Just <$> (char '=' *> many urlSafeChar)))

extractInnerHtml :: Text -> Parser (String)
extractInnerHtml elm = do
  manyTill anyChar (try $ string ("<" <> elm <> ">"))
  inner <- manyTill anyChar (try $ string ("</" <> elm <> ">"))
  return inner

extractInnerHtmlMany :: Text -> Parser ([String])
extractInnerHtmlMany = option [] . many . try . extractInnerHtml

sumNumber :: Parser Int
sumNumber = fmap sum $ many $ takeTill isDigit *> signed decimal

-- markdownもどき
type Doc = [BlockNode]
data BlockNode = Header Int Text | TextNode Text | ListNode [Text]
  deriving (Show)

markdown :: Parser Doc
markdown = do
  many $ header <|> list <|> text'
  where
    line = P.takeWhile (not . isEndOfLine) <* endOfLine
    spaces1 = many1 space
    header = do
      lv <- length <$> many1 (char '#')
      spaces1
      x <- line
      pure $ Header lv x
    list = do
      ListNode <$> many1 (char '-' *> spaces1 *> line)
    text' = do
      TextNode <$> line
--
