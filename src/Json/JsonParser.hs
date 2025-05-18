{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- JsonParser
-}

module Json.JsonParser
  ( parseJson
  , JsonValue(..)
  ) where

import Document
import Parser

data JsonValue
  = JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show)

myLookup :: Eq k => k -> [(k,v)] -> Maybe v
myLookup _ [] = Nothing
myLookup key ((k,v):rest)
  | key == k = Just v
  | otherwise = myLookup key rest

myMapM :: (a -> Maybe b) -> [a] -> Maybe [b]
myMapM _ [] = Just []
myMapM f (x:xs) = do
  y <- f x
  ys <- myMapM f xs
  return (y:ys)

parseJson :: String -> Maybe Document
parseJson input = do
  (_, r1) <- skipSpaces input
  (jv, r2) <- parseJsonObject r1
  (_, _) <- skipSpaces r2
  buildDocument jv

parseJsonObject :: String -> Maybe (JsonValue, String)
parseJsonObject input = do
  (_, r1) <- parseChar '{' input
  (_, r2) <- skipSpaces r1
  (pairs, r3) <- parsePairs r2
  (_, r4) <- skipSpaces r3
  (_, r5) <- parseChar '}' r4
  return (JsonObject pairs, r5)

parsePairs :: String -> Maybe ([(String, JsonValue)], String)
parsePairs input = do
  (_, r1) <- skipSpaces input
  parseKeyValue r1

parseKeyValue :: String -> Maybe ([(String, JsonValue)], String)
parseKeyValue input = case parseJsonString input of
  Just (key, r2) -> parseValue key r2
  Nothing -> return ([], input)

parseValue :: String -> String -> Maybe ([(String, JsonValue)], String)
parseValue key input = do
  (_, r3) <- skipSpaces input
  (_, r4) <- parseChar ':' r3
  (_, r5) <- skipSpaces r4
  (v, r6) <- parseJsonValue r5
  parseRest key v r6

parseRest :: String -> JsonValue -> String -> Maybe ([(String, JsonValue)], String)
parseRest key v input = do
  (_, r7) <- skipSpaces input
  case parseChar ',' r7 of
    Just (_, r8) -> do
      (rest, r9) <- parsePairs r8
      return ((key, v) : rest, r9)
    Nothing -> return ([(key, v)], r7)


parseJsonString :: String -> Maybe (String, String)
parseJsonString input = do
  (_, r1) <- parseChar '"' input
  let (s, r2) = span (/= '"') r1
  (_, r3) <- parseChar '"' r2
  return (s, r3)

parseJsonArray :: String -> Maybe (JsonValue, String)
parseJsonArray input = do
  (_, r1) <- parseChar '[' input
  (_, r2) <- skipSpaces r1
  (vals, r3) <- parseArrayValues r2
  (_, r4) <- skipSpaces r3
  (_, r5) <- parseChar ']' r4
  return (JsonArray vals, r5)

parseArrayValues :: String -> Maybe ([JsonValue], String)
parseArrayValues input = do
  (_, r1) <- skipSpaces input
  parseArrayElement r1

parseArrayElement :: String -> Maybe ([JsonValue], String)
parseArrayElement input = case parseJsonValue input of
  Just (v, r2) -> parseArrayTail v r2
  Nothing -> return ([], input)

parseArrayTail :: JsonValue -> String -> Maybe ([JsonValue], String)
parseArrayTail v input = do
  (_, r3) <- skipSpaces input
  case parseChar ',' r3 of
    Just (_, r4) -> do
      (rest, r5) <- parseArrayValues r4
      return (v : rest, r5)
    Nothing -> return ([v], r3)

parseJsonValue :: String -> Maybe (JsonValue, String)
parseJsonValue input = do
  (_, r) <- skipSpaces input
  case r of
    ('"':_) -> do
      (s, r') <- parseJsonString r
      return (JsonString s, r')
    ('{':_) -> parseJsonObject r
    ('[':_) -> parseJsonArray r
    _ -> Nothing

buildDocument :: JsonValue -> Maybe Document
buildDocument (JsonObject fields) = do
  headerJv <- myLookup "header" fields
  bodyJv <- myLookup "body" fields
  hdr <- buildHeader headerJv
  cs <- buildBody bodyJv
  return Document { header = hdr, body = cs }
buildDocument _ = Nothing

buildHeader :: JsonValue -> Maybe Header
buildHeader (JsonObject fields) = do
  JsonString t <- myLookup "title" fields
  let a = extractMaybeField "author" fields
  let d = extractMaybeField "date" fields
  return Header { headerTitle = t, headerAuthor = a, headerDate = d }
buildHeader _ = Nothing

extractMaybeField :: String -> [(String, JsonValue)] -> Maybe String
extractMaybeField key fields = case myLookup key fields of
  Just (JsonString s) -> Just s
  _ -> Nothing

buildBody :: JsonValue -> Maybe [Content]
buildBody (JsonArray items) = myMapM buildContent items
buildBody _ = Nothing

buildContent :: JsonValue -> Maybe Content
buildContent (JsonString txt) = Just (Paragraph [PlainText txt])
buildContent (JsonArray arr) = do
  inls <- extractInlines arr
  return (Paragraph inls)
buildContent (JsonObject fields)
  | isSection fields = buildSection fields
  | isCodeBlock fields = buildCodeBlock fields
  | isList fields = buildList fields
  | isParagraph fields = buildParagraph fields
  | otherwise = fallbackInline fields
buildContent _ = Nothing

isSection :: [(String, JsonValue)] -> Bool
isSection fields = case myLookup "section" fields of
  Just (JsonObject _) -> True
  _ -> False

buildSection :: [(String, JsonValue)] -> Maybe Content
buildSection fields = do
  JsonObject sec <- myLookup "section" fields
  JsonString t <- myLookup "title" sec
  JsonArray contArr <- myLookup "content" sec
  cs <- myMapM buildContent contArr
  return (Section t cs)

isCodeBlock :: [(String, JsonValue)] -> Bool
isCodeBlock fields = case myLookup "codeblock" fields of
  Just (JsonString _) -> True
  _ -> False

buildCodeBlock :: [(String, JsonValue)] -> Maybe Content
buildCodeBlock fields = do
  JsonString cb <- myLookup "codeblock" fields
  return (CodeBlock cb)

isList :: [(String, JsonValue)] -> Bool
isList fields = case myLookup "list" fields of
  Just (JsonArray _) -> True
  _ -> False

buildList :: [(String, JsonValue)] -> Maybe Content
buildList fields = do
  JsonArray items <- myLookup "list" fields
  its <- myMapM parseListItem items
  return (List its)

isParagraph :: [(String, JsonValue)] -> Bool
isParagraph fields = case myLookup "paragraph" fields of
  Just (JsonArray _) -> True
  _ -> False

buildParagraph :: [(String, JsonValue)] -> Maybe Content
buildParagraph fields = do
  JsonArray arr <- myLookup "paragraph" fields
  inls <- extractInlines arr
  return (Paragraph inls)

fallbackInline :: [(String, JsonValue)] -> Maybe Content
fallbackInline fields = do
  inl <- extractInline (JsonObject fields)
  return (Paragraph [inl])

parseListItem :: JsonValue -> Maybe Item
parseListItem (JsonObject fields) = do
  JsonArray arr <- myLookup "item" fields
  contents <- myMapM buildContent arr
  return (Item contents)
parseListItem _ = Nothing

extractInline :: JsonValue -> Maybe Inline
extractInline (JsonString s) = Just (PlainText s)
extractInline (JsonObject fields)
  | Just (JsonString s) <- myLookup "bold" fields = Just (Bold s)
  | Just (JsonString s) <- myLookup "italic" fields = Just (Italic s)
  | Just (JsonString s) <- myLookup "code" fields = Just (Code s)
  | Just (JsonObject lf) <- myLookup "link" fields =
      buildLink lf
  | Just (JsonObject lf) <- myLookup "image" fields =
      buildImage lf
  | otherwise = Nothing
extractInline _ = Nothing

buildLink :: [(String, JsonValue)] -> Maybe Inline
buildLink lf = do
  JsonString url <- myLookup "url" lf
  JsonArray arr <- myLookup "content" lf
  inls <- extractInlines arr
  let txt = concat [t | PlainText t <- inls]
  return (Link url txt)

buildImage :: [(String, JsonValue)] -> Maybe Inline
buildImage lf = do
  JsonString url <- myLookup "url" lf
  JsonArray arr <- myLookup "alt" lf
  inls <- extractInlines arr
  let txt = concat [t | PlainText t <- inls]
  return (Image url txt)

extractInlines :: [JsonValue] -> Maybe [Inline]
extractInlines [] = Just []
extractInlines (x:xs) = do
  i <- extractInline x
  is <- extractInlines xs
  return (i:is)