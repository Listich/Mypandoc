{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- Document
-}

module Document (
  Document(..),
  Header(..),
  Content(..),
  Item(..),
  Inline(..)
) where

data Document = Document
  { header :: Header
  , body   :: [Content]
  } deriving (Show, Eq)

data Header = Header
  { headerTitle  :: String
  , headerAuthor :: Maybe String
  , headerDate   :: Maybe String
  } deriving (Show, Eq)

data Content
  = Paragraph [Inline]
  | Section String [Content]
  | CodeBlock String
  | List [Item]
  deriving (Show, Eq)

data Item = Item [Content]
  deriving (Show, Eq)

data Inline
  = PlainText String
  | Link String String
  | Bold String
  | Italic String
  | Code String
  | Image String String 
  deriving (Show, Eq)