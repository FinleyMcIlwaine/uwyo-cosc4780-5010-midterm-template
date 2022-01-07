{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (foldM)
import Data.Char
import Data.List
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    "haskell" : "grad" : _ -> toTestProcess "haskell" >>= run True
    "haskell" : _ -> toTestProcess "haskell" >>= run False
    "java" : "grad" : _ -> toTestProcess "java" >>= run True
    "java" : _ -> toTestProcess "java" >>= run False
    _ -> die "tester: bad arguments\nusage: tester <haskell | java>"

toTestProcess :: String -> IO CreateProcess
toTestProcess s = do
  wd <- getCurrentDirectory
  return $
    CreateProcess
      { cmdspec = ShellCommand "make compile",
        cwd = Just (takeDirectory wd </> s),
        env = Nothing,
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe,
        close_fds = False,
        create_group = False,
        delegate_ctlc = False,
        detach_console = False,
        create_new_console = False,
        new_session = False,
        child_group = Nothing,
        child_user = Nothing,
        use_process_jobs = False
      }

run :: Bool -> CreateProcess -> IO ()
run testGrad p =
  let total = if testGrad then 100 else 80
   in do
        good <-
          map (ExitSuccess,) . sortOn ((read :: String -> Int) . takeWhile isDigit . takeFileName)
            <$> ( (++) <$> (map ("good" </>) <$> listDirectory "good")
                    <*> if testGrad then map (("grad" </>) . ("good" </>)) <$> listDirectory ("grad" </> "good") else pure []
                )
        bad <-
          map (ExitFailure 2,) . sortOn ((read :: String -> Int) . takeWhile isDigit . takeFileName)
            <$> ( (++) <$> (map ("bad" </>) <$> listDirectory "bad")
                    <*> if testGrad then map (("grad" </>) . ("bad" </>)) <$> listDirectory ("grad" </> "bad") else pure []
                )
        score <- foldM (runFile testGrad p) 0 (good ++ bad)
        putStrLn ""
        setSGR [SetColor Foreground Vivid Blue]
        putStrLn $ "testing complete, final (tentative) score: " ++ show score ++ "/" ++ show total
        setSGR [Reset]

runFile :: Bool -> CreateProcess -> Int -> (ExitCode, FilePath) -> IO Int
runFile grad process score (expect, filename) =
  let total = if grad then 100 else 80
   in do
        putStrLn $ "typchecking file: " ++ filename ++ " ... "
        source <- readFile filename
        (Just stdin, Just stdout, Just stderr, ph) <- createProcess process
        hPutStr stdin source
        exitCode <- waitForProcess ph
        output <- hGetContents stdout
        if exitCode `matches` expect
          then do
            setSGR [SetColor Foreground Dull Green]
            putStrLn $ "typecheck success! score: " ++ show (score + 2) ++ "/" ++ show total
            setSGR [Reset]
            return $ score + 2
          else do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn $ "typecheck failure! score: " ++ show score ++ "/" ++ show total
            setSGR [Reset]
            putStrLn $ "    expected typechecking to " ++ (if expect == ExitSuccess then "pass, but it failed!" else "fail, but it passed!")
            putStrLn "    source program:"
            putStrLn (unlines . map ("    " ++) $ lines source)
            putStrLn "    typechecker standard output:"
            putStrLn (unlines . map ("    " ++) $ lines output)
            return score

matches :: ExitCode -> ExitCode -> Bool
matches ExitSuccess ExitSuccess = True
matches (ExitFailure _) (ExitFailure _) = True
matches _ _ = False
