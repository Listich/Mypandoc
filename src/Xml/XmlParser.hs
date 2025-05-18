{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- XmlParser
-}

module Xml.XmlParser (parseXml) where

import Document
import Parser
import Control.Applicative ((<|>))

parseLinkInline :: Parser Inline
parseLinkInline = \input -> do
  (_, rest1) <- parseString "<link " input
  (urlVal, rest2) <- parseAttribute "url" rest1
  (_, rest3) <- parseString ">" rest2
  (txt, rest4) <- parseUntil '<' rest3
  (_, rest5) <- parseString "</link>" rest4
  return (Link urlVal txt, rest5)

parseImageInline :: Parser Inline
parseImageInline = \input -> do
  (_, rest1) <- parseString "<image " input
  (urlVal, rest2) <- parseAttribute "url" rest1
  (_, rest3) <- parseString ">" rest2
  (txt, rest4) <- parseUntil '<' rest3
  (_, rest5) <- parseString "</image>" rest4
  return (Image urlVal txt, rest5)

parseSimpleInline :: (String -> Inline) -> String -> Parser Inline
parseSimpleInline constructor tagName = \input -> do
  (_, rest1) <- parseString ("<" ++ tagName ++ ">") input
  (content, rest2) <- parseUntil '<' rest1
  (_, rest3) <- parseString ("</" ++ tagName ++ ">") rest2
  return (constructor content, rest3)

parsePlainTextUntilSpecial :: Parser Inline
parsePlainTextUntilSpecial = \input ->
  let (txt, rest) = span (/= '<') input
  in if null txt then Nothing else Just (PlainText txt, rest)

parseOneInline :: Parser Inline
parseOneInline = \input -> do
  (_, input') <- skipSpaces input
  parseInlineByTag input'

parseInlineByTag :: String -> Maybe (Inline, String)
parseInlineByTag input
  | "<link " `startsWith` input = parseLinkInline input
  | "<bold>" `startsWith` input = parseSimpleInline Bold "bold" input
  | "<italic>" `startsWith` input = parseSimpleInline Italic "italic" input
  | "<code>" `startsWith` input = parseSimpleInline Code "code" input
  | "<image " `startsWith` input = parseImageInline input
  | otherwise = parsePlainTextUntilSpecial input

parseInlinesUntilEndParagraph :: Parser [Inline]
parseInlinesUntilEndParagraph = \input -> do
  (_, input') <- skipSpaces input
  if "</paragraph>" `startsWith` input'
    then Just ([], drop (length "</paragraph>") input')
    else do
      (inline, rest1) <- parseOneInline input'
      (inlines, rest2) <- parseInlinesUntilEndParagraph rest1
      return (inline : inlines, rest2)

parseParagraphContent :: Parser [Inline]
parseParagraphContent = \input -> do
  (_, rest1) <- parseString "<paragraph>" input
  parseInlinesUntilEndParagraph rest1

parseListItems :: Parser [Item]
parseListItems = \input -> do
  (_, rest1) <- skipSpaces input
  case parseParagraphContent rest1 of
    Just (inlines, rest2) -> do
      (others, rest3) <- parseListItems rest2
      return (Item [Paragraph inlines] : others, rest3)
    Nothing -> Just ([], input)

parseList :: Parser Content
parseList = \input -> do
  (_, rest1) <- parseString "<list>" input
  (_, rest2) <- skipSpaces rest1
  (items, rest3) <- parseListItems rest2
  (_, rest4) <- skipSpaces rest3
  (_, rest5) <- parseString "</list>" rest4
  return (List items, rest5)

parseCodeBlock :: Parser Content
parseCodeBlock = \input -> do
  (_, rest1) <- parseString "<codeblock>" input
  (_, rest2) <- skipSpaces rest1
  (paragraph, rest3) <- parseTag "paragraph" rest2
  (_, rest4) <- skipSpaces rest3
  (_, rest5) <- parseString "</codeblock>" rest4
  return (CodeBlock paragraph, rest5)

parseSection :: Parser Content
parseSection = \input -> do
  (_, rest1) <- parseString "<section " input
  (titleVal, rest2) <- parseAttribute "title" rest1
  (_, rest3) <- parseString ">" rest2
  (_, rest4) <- skipSpaces rest3
  (paragraphs, rest5) <- parseManyContents rest4
  (_, rest6) <- skipSpaces rest5
  (_, rest7) <- parseString "</section>" rest6
  return (Section titleVal paragraphs, rest7)

parseContent :: Parser Content
parseContent = \input -> do
  (_, rest) <- skipSpaces input
  parseSection rest
    <|> parseCodeBlock rest
    <|> parseList rest
    <|> parseAsParagraph rest

parseAsParagraph :: Parser Content
parseAsParagraph input = do
  (inlines, rest) <- parseParagraphContent input
  return (Paragraph inlines, rest)

parseManyContents :: Parser [Content]
parseManyContents = \input -> do
  (_, rest) <- skipSpaces input
  case parseContent rest of
    Just (c, rest1) -> do
      (cs, rest2) <- parseManyContents rest1
      return (c : cs, rest2)
    Nothing -> Just ([], input)

parseBodyTag :: Parser [Content]
parseBodyTag = \input -> do
  (_, rest1) <- parseString "<body>" input
  (_, rest2) <- skipSpaces rest1
  (contents, rest3) <- parseManyContents rest2
  (_, rest4) <- skipSpaces rest3
  (_, rest5) <- parseString "</body>" rest4
  return (contents, rest5)

parseAuthor :: String -> (Maybe String, String)
parseAuthor input = case parseTag "author" input of
  Just (a, r) -> (Just a, r)
  Nothing -> (Nothing, input)

parseDate :: String -> (Maybe String, String)
parseDate input = case parseTag "date" input of
  Just (d, r) -> (Just d, r)
  Nothing -> (Nothing, input)

parseHeaderTag :: Parser Header
parseHeaderTag = \input -> do
  (_, afterOpen) <- parseString "<header " input
  (titleVal, afterTitle) <- parseAttribute "title" afterOpen
  (_, afterTitleTag) <- parseString ">" afterTitle
  parseHeaderFields titleVal afterTitleTag

parseHeaderFields :: String -> String -> Maybe (Header, String)
parseHeaderFields titleVal input = do
  (_, afterSpace1) <- skipSpaces input
  let (authorVal, afterAuthor) = parseAuthor afterSpace1
  (_, afterSpace2) <- skipSpaces afterAuthor
  let (dateVal, afterDate) = parseDate afterSpace2
  (_, afterSpace3) <- skipSpaces afterDate
  (_, rest) <- parseString "</header>" afterSpace3
  return (Header titleVal authorVal dateVal, rest)


parseXml :: String -> Maybe Document
parseXml input = do
  (_, rest) <- parseString "<document>" input
  parseXmlBody rest

parseXmlBody :: String -> Maybe Document
parseXmlBody input = do
  (_, rest1) <- skipSpaces input
  (hdr, rest2) <- parseHeaderTag rest1
  (_, rest3) <- skipSpaces rest2
  (bodyVal, _) <- parseBodyTag rest3
  return $ Document { header = hdr, body = bodyVal }

startsWith :: String -> String -> Bool
startsWith prefix str = prefix == take (length prefix) str
