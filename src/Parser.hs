{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- Parser
-}

module Parser (
  Parser,
  parseChar,
  parseString,
  parseUntil,
  parseTag,
  parseAttribute,
  skipSpaces
) where

import Data.List (isPrefixOf)

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar c = \input ->
  case input of
    (x:xs) -> if x == c then Just (x, xs) else Nothing
    [] -> Nothing

parseString :: String -> Parser String
parseString "" = \input -> Just ("", input)
parseString (x:xs) = \input -> do
  (c, rest1) <- parseChar x input
  (cs, rest2) <- parseString xs rest1
  return (c:cs, rest2)

parseUntil :: Char -> Parser String
parseUntil stop = \input ->
  let (txt, rest) = span (/= stop) input
  in Just (txt, rest)

parseAttribute :: String -> Parser String
parseAttribute attrName = \input -> do
  (_, rest1) <- parseString (attrName ++ "=\"") input
  (value, rest2) <- parseUntil '"' rest1
  (_, rest3) <- parseChar '"' rest2
  return (value, rest3)

skipSpaces :: Parser ()
skipSpaces = \input ->
  let rest = dropWhile (`elem` " \t\r\n") input
  in Just ((), rest)

parseTag :: String -> Parser String
parseTag tag = \input -> do
  (_, rest1) <- parseString ("<" ++ tag ++ ">") input
  let closing = "</" ++ tag ++ ">"
  let (content, rest2) = breakOn closing rest1
  (_, rest3) <- parseString closing rest2
  return (content, rest3)

breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn needle (x:xs)
  | needle `isPrefixOf` (x:xs) = ("", x:xs)
  | otherwise =
      let (before, after) = breakOn needle xs
      in (x:before, after)
