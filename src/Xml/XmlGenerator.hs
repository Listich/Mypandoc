{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- XmlGenerator
-}

module Xml.XmlGenerator (documentToXml) where

import Document

documentToXml :: Document -> String
documentToXml (Document h b) =
  "<document>\n"
    ++ headerToXml h
    ++ bodyToXml b
    ++ "</document>"

headerToXml :: Header -> String
headerToXml (Header title author date) =
  "  <header title=" ++ show title ++ ">\n"
    ++ maybe "" (\a -> "    <author>" ++ a ++ "</author>\n") author
    ++ maybe "" (\d -> "    <date>" ++ d ++ "</date>\n") date
    ++ "  </header>\n"

bodyToXml :: [Content] -> String
bodyToXml [] = "  <body>\n  </body>\n"
bodyToXml cs = "  <body>\n"
  ++ concatMap contentToXml cs
  ++ "  </body>\n"

contentToXml :: Content -> String
contentToXml (Paragraph inls) =
  "    <paragraph>" ++ concatMap inlineToXml inls ++ "</paragraph>\n"
contentToXml (CodeBlock txt) =
  "    <codeblock>\n"
    ++ "      <paragraph>" ++ txt ++ "</paragraph>\n"
    ++ "    </codeblock>\n"
contentToXml (List items) =
  "    <list>\n"
    ++ concatMap itemToXml items
    ++ "    </list>\n"
contentToXml (Section title contents) =
  "    <section title=" ++ show title ++ ">\n"
    ++ concatMap contentToXml contents
    ++ "    </section>\n"

itemToXml :: Item -> String
itemToXml (Item contents) =
  "      <item>\n"
    ++ concatMap contentToXml contents
    ++ "      </item>\n"

inlineToXml :: Inline -> String
inlineToXml (PlainText txt) = txt
inlineToXml (Bold txt) = "<bold>" ++ txt ++ "</bold>"
inlineToXml (Italic txt) = "<italic>" ++ txt ++ "</italic>"
inlineToXml (Code txt) = "<code>" ++ txt ++ "</code>"
inlineToXml (Link url txt) =
  "<link url=" ++ show url ++ ">" ++ txt ++ "</link>"
inlineToXml (Image url alt) =
  "<image url=" ++ show url ++ ">" ++ alt ++ "</image>"
