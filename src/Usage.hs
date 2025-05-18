{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-serena.kifoula
-- File description:
-- Usage
-}

module Usage (printUsage) where

printUsage :: IO ()
printUsage =
  putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]" >>
  putStrLn "" >>
  putStrLn " ifile    path to the file to convert" >>
  putStrLn " oformat  output format (xml, json, markdown)" >>
  putStrLn " ofile    path to the output file" >>
  putStrLn " iformat  input format (xml, json, markdown)"