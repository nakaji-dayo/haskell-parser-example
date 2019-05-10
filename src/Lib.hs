module Lib where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

test = do
  run date "date" "2019-04-21"
  run date "date" "hoge"
  run date' "date'" "2019-04-21"
  run date' "date'" "hoge"
  run url "url" "http://yahoo.co.jp"
  run url "url" "hoge+zzz://fuga.com/x-y/z?a=b&c&d=8"
  let html = "<p><code>manyTill p end</code> applies parser <code>p</code> <em>zero</em> or more times until parser <code>end</code> succeeds. Returns the list of values returned by <code>p</code>. <a href=\"https://hackage.haskell.org/package/parsec-3.1.13.0\">ref</a> </p>"
  run ((extractInnerHtml "code")) "extractInnerHtml" html
  run (extractInnerHtml "code") "extractInnerHtml" "hogehoge"
  run ((extractInnerHtmlMany "code")) "extractInnerHtmlMany" html
  run (extractInnerHtmlMany "code") "extractInnerHtmlMany" "hogehoge"

run :: Show a => Parser a -> String -> String -> IO ()
run parser name input = do
  putStrLn $ concat ["----", name, "----"]
  print $ parse parser name input

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
  s <- many (letter <|> oneOf "+-")
  string "://"
  d <- many urlSafeChar
  ps <- many $ char '/' *> many urlSafeChar
  qs <- option [] $ do
    char '?'
    pair `sepBy` char '&'
  return (s, d, ps, qs)

urlSafeChar :: Parser Char
urlSafeChar = letter <|> digit <|> oneOf ".~_-"

pair :: Parser ([Char], Maybe [Char])
pair = (,) <$> many urlSafeChar <*> (optionMaybe $ char '=' *> many urlSafeChar)

extractInnerHtml :: String -> Parser (String)
extractInnerHtml elm = do
  manyTill anyChar (try $ string ("<" <> elm <> ">"))
  inner <- manyTill anyChar (try $ string ("</" <> elm <> ">"))
  return inner

extractInnerHtmlMany :: String -> Parser ([String])
extractInnerHtmlMany = option [] . many . try . extractInnerHtml
