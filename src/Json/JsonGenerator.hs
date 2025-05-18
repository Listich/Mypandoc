{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- JsonGenerator
-}

module Json.JsonGenerator (documentToJson) where

import Document
import Data.List (intercalate)

documentToJson :: Document -> String
documentToJson doc =
  "{\n"
    ++ "  \"header\": " ++ headerToJson (header doc) ++ ",\n"
    ++ "  \"body\": " ++ bodyToJson (body doc) ++ "\n"
    ++ "}"

headerToJson :: Header -> String
headerToJson (Header t a d) =
  "{\n"
    ++ "    \"title\": " ++ show t
    ++ maybe "" (\x -> ",\n    \"author\": " ++ show x) a
    ++ maybe "" (\x -> ",\n    \"date\": " ++ show x) d
    ++ "\n  }"

bodyToJson :: [Content] -> String
bodyToJson [] = "[]"
bodyToJson cs =
  "[\n"
    ++ intercalate ",\n" (map (indent 4 . contentToJson) cs)
    ++ "\n  ]"

contentToJson :: Content -> String
contentToJson (Paragraph inls) =
  "{ \"paragraph\": ["
    ++ intercalate ", " (map inlineToJson inls)
    ++ "] }"
contentToJson (CodeBlock txt) =
  "{ \"codeblock\": " ++ show txt ++ " }"
contentToJson (List items) =
  "{ \"list\": [\n"
    ++ intercalate ",\n" (map (indent 2 . itemToJson) items)
    ++ "\n    ] }"
contentToJson (Section title contents) =
  "{ \"section\": {\n"
    ++ "      \"title\": " ++ show title ++ ",\n"
    ++ "      \"content\": [\n"
    ++ intercalate ",\n" (map (indent 8 . contentToJson) contents)
    ++ "\n      ]\n    }}"

inlineToJson :: Inline -> String
inlineToJson (PlainText txt) = show txt
inlineToJson (Bold txt) = "{ \"bold\": " ++ show txt ++ " }"
inlineToJson (Italic txt) = "{ \"italic\": " ++ show txt ++ " }"
inlineToJson (Code txt) = "{ \"code\": " ++ show txt ++ " }"
inlineToJson (Link url txt) =
  "{ \"link\": {\n"
    ++ "  \"url\": " ++ show url ++ ",\n"
    ++ "  \"content\": " ++ show txt ++ "\n}"
    ++ " }"
inlineToJson (Image url alt) =
  "{ \"image\": {\n"
    ++ "  \"url\": " ++ show url ++ ",\n"
    ++ "  \"alt\": " ++ show alt ++ "\n}"
    ++ " }"

itemToJson :: Item -> String
itemToJson (Item contents) =
  "[ " ++ intercalate ", " (map contentToJson contents) ++ " ]"

indent :: Int -> String -> String
indent n str =
  unlines (map (\line -> replicate n ' ' ++ line) (lines str))