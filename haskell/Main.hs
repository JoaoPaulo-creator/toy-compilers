module Main where

import CodeGen
import Lexer
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      content <- readFile inputFile
      let tokens = lexer content
      case parse tokens of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
          let assembly = generateCode ast
          writeFile "output.s" assembly
          putStrLn "Compilation successful. Assembly output written to output.s"
    _ -> putStrLn "Usage: toycompiler <input.toy>"
