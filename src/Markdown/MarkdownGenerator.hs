{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- MarkdownGenerator
-}

module Markdown.MarkdownGenerator (documentToMarkdown) where

import Document
import Data.List (intercalate)

documentToMarkdown :: Document -> String
documentToMarkdown doc =
  let hdr = header doc
      bodyStr = formatBody (body doc)
      headerPart = formatHeader hdr
  in if null bodyStr
     then headerPart
     else headerPart ++ "\n\n" ++ bodyStr

formatHeader :: Header -> String
formatHeader hdr =
  "---\n"
    ++ "title: " ++ headerTitle hdr ++ "\n"
    ++ maybe "" (\a -> "author: " ++ a ++ "\n") (headerAuthor hdr)
    ++ maybe "" (\d -> "date: " ++ d ++ "\n") (headerDate hdr)
    ++ "---"

formatBody :: [Content] -> String
formatBody =
  stripTrailingEmptyLines . bodyToMarkdown

stripTrailingEmptyLines :: String -> String
stripTrailingEmptyLines =
  unlines . reverse . dropWhile null . reverse . lines

bodyToMarkdown :: [Content] -> String
bodyToMarkdown =
  intercalate "\n\n" . map (rstripNewlines . contentToMarkdown 1)

contentToMarkdown :: Int -> Content -> String
contentToMarkdown _ (Paragraph inls) =
  inlinesToMarkdown inls
contentToMarkdown _ (CodeBlock txt) =
  "```\n" ++ txt ++ "\n```"
contentToMarkdown level (Section title contents) =
  replicate level '#' ++ " " ++ title ++ "\n\n"
    ++ intercalate "\n\n"
         (map (rstripNewlines . contentToMarkdown (level + 1)) contents)
contentToMarkdown level (List items) =
  itemsToMarkdown items level

itemsToMarkdown :: [Item] -> Int -> String
itemsToMarkdown items level =
  intercalate "\n" (map (\item -> itemToMarkdown item level) items)

itemToMarkdown :: Item -> Int -> String
itemToMarkdown (Item contents) level =
  intercalate "\n"
    (map (rstripNewlines . contentToMarkdownSimpleIndent level) contents)

contentToMarkdownSimpleIndent :: Int -> Content -> String
contentToMarkdownSimpleIndent level (Paragraph inlines) =
  replicate ((level - 1) * 2) ' ' ++ "- " ++ inlinesToMarkdown inlines
contentToMarkdownSimpleIndent level (List items) =
  itemsToMarkdown items (level + 1)
contentToMarkdownSimpleIndent _ _ = ""

inlinesToMarkdown :: [Inline] -> String
inlinesToMarkdown = concatMap inlineToMarkdown

inlineToMarkdown :: Inline -> String
inlineToMarkdown (PlainText txt) = txt
inlineToMarkdown (Link url txt) =
  "[" ++ txt ++ "](" ++ url ++ ")"
inlineToMarkdown (Bold txt) =
  "**" ++ txt ++ "**"
inlineToMarkdown (Italic txt) =
  "*" ++ txt ++ "*"
inlineToMarkdown (Code txt) =
  "`" ++ txt ++ "`"
inlineToMarkdown (Image url alt) =
  "![" ++ alt ++ "](" ++ url ++ ")"

rstripNewlines :: String -> String
rstripNewlines =
  reverse . dropWhile (== '\n') . reverse