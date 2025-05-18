{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- Main module
-}

module Main (main) where

import Usage (printUsage)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Document
import Xml.XmlParser (parseXml)
import Json.JsonParser (parseJson)
import Markdown.MarkdownGenerator (documentToMarkdown)
import Xml.XmlGenerator (documentToXml)
import Json.JsonGenerator (documentToJson)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Control.Monad (when)
import Control.Exception (catch, IOException)

data Options = Options
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , inputFormat :: Maybe String
  , outputFormat :: Maybe String
  } deriving (Show)

parseArgs :: [String] -> Either String Options
parseArgs args = go args (Options Nothing Nothing Nothing Nothing)
  where
    go [] opts = Right opts
    go ("-i":f:rest) opts = go rest opts { inputFile = Just f }
    go ("-o":f:rest) opts = go rest opts { outputFile = Just f }
    go ("-e":f:rest) opts = go rest opts { inputFormat = Just f }
    go ("-f":f:rest) opts = go rest opts { outputFormat = Just f }
    go (flag:_) _ = Left ("Unknown or incomplete option: " ++ flag)

detectFormat :: FilePath -> Maybe String -> String -> String
detectFormat _ (Just fmt) _ = map toLower fmt
detectFormat path Nothing content
  | ".xml" `isSuffixOfCI` path = "xml"
  | ".json" `isSuffixOfCI` path = "json"
  | ".md" `isSuffixOfCI` path = "markdown"
  | startsWith "<" content = "xml"
  | startsWith "{" content = "json"
  | otherwise = "unknown"

startsWith :: String -> String -> Bool
startsWith prefix str =
  prefix == take (length prefix) (dropWhile (== ' ') str)

isSuffixOfCI :: String -> String -> Bool
isSuffixOfCI suffix str =
  map toLower suffix `isSuffixOf` map toLower str

readInputFile :: FilePath -> IO String
readInputFile path = readFile path `catch` handle
  where
    handle :: IOException -> IO String
    handle _ =
      hPutStrLn stderr ("Error: Cannot open file: " ++ path)
      >> exitWith (ExitFailure 84)

parseDocument :: String -> String -> Maybe Document
parseDocument "xml" = parseXml
parseDocument "json" = parseJson
parseDocument "markdown" = const Nothing
parseDocument _ = const Nothing

writeOutput :: String -> Maybe FilePath -> IO ()
writeOutput content (Just path) = writeFile path content
writeOutput content Nothing = putStr content

selectOutputFormat :: String -> Document -> Either String String
selectOutputFormat format doc
  | format `elem` ["json", "xml", "markdown"] = Right $ case format of
      "json" -> documentToJson doc
      "xml" -> documentToXml doc
      "markdown" -> documentToMarkdown doc
      _ -> ""
  | otherwise = Left ("Error: Invalid output format: " ++ format)

exitWithError :: String -> IO ()
exitWithError msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure 84)

handleRun :: Options -> IO ()
handleRun (Options (Just i) outFile inFormat (Just outFormat)) =
  processFile i outFile inFormat outFormat
handleRun _ =
  hPutStrLn stderr "Error: Missing or incorrect options."
  >> printUsage
  >> exitWith (ExitFailure 84)

processFile :: FilePath -> Maybe FilePath -> Maybe String -> String -> IO ()
processFile i outFile inFormat outFormat = do
  content <- readInputFile i
  let inputFmt = detectFormat i inFormat content
  validateFormats inputFmt i outFile
  convertContent inputFmt content outFormat outFile

validateFormats :: String -> FilePath -> Maybe FilePath -> IO ()
validateFormats "unknown" _ _ =
  exitWithError "Error: Unknown input format and not specified."
validateFormats _ i (Just o) | i == o =
  exitWithError "Error: Input and output files must be different."
validateFormats _ _ _ = return ()

convertContent :: String -> String -> String -> Maybe FilePath -> IO ()
convertContent inputFmt content outFormat outFile =
  case parseDocument inputFmt content of
    Just doc -> case selectOutputFormat (map toLower outFormat) doc of
      Right result -> writeOutput result outFile
      Left err -> exitWithError err
    Nothing -> exitWithError "Error: Parsing failed."

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err ->
      hPutStrLn stderr err >> printUsage
      >> exitWith (ExitFailure 84)
    Right opts -> handleRun opts
