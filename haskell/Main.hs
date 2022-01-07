module Main where

import ParCPP ( pProgram, myLexer )
import System.Exit (die)

main :: IO ()
main = do
  c <- getContents
  case pProgram (myLexer c) of
    Left err -> putStrLn "parse error!" >> die err
    Right tree -> undefined -- successful parse! typecheck here
