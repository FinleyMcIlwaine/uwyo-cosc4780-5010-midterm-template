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
          map (ExitSuccess,) . sortOn ((read :: String -> Int) . takeWhile isDigit . takeFileName . fst)
            <$> ( (++) <$> (map ((,2) . ("good" </>)) <$> listDirectory "good")
                    <*> if testGrad then map ((,1) . ("grad" </>) . ("good" </>)) <$> listDirectory ("grad" </> "good") else pure []
                )
        bad <-
          map (ExitFailure 2,) . sortOn ((read :: String -> Int) . takeWhile isDigit . takeFileName . fst)
            <$> ( (++) <$> (map ((,2) . ("bad" </>)) <$> listDirectory "bad")
                    <*> if testGrad then map ((,1) . ("grad" </>) . ("bad" </>)) <$> listDirectory ("grad" </> "bad") else pure []
                )
        let total = sum $ map (snd . snd) (bad ++ good)
        score <- foldM (runFile total p) 0 (good ++ bad)
        putStrLn ""
        setSGR [SetColor Foreground Vivid Blue]
        putStrLn $ "testing complete, final (tentative) score: " ++ show score ++ "/" ++ show total
        setSGR [Reset]

runFile :: Int -> CreateProcess -> Int -> (ExitCode, (FilePath, Int)) -> IO Int
runFile total process score (expect, (filename, worth)) =
  do
    putStrLn $ "typchecking file: " ++ filename ++ " ... "
    source <- readFile filename
    (Just stdin, Just stdout, Just stderr, ph) <- createProcess process
    hPutStr stdin source
    exitCode <- waitForProcess ph
    output <- hGetContents stdout
    if exitCode `matches` expect
      then do
        setSGR [SetColor Foreground Dull Green]
        putStrLn $ "type check success! score: " ++ show (score + worth) ++ "/" ++ show total
        setSGR [Reset]
        return $ score + worth
      else do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "type check failure! score: " ++ show score ++ "/" ++ show total
        setSGR [Reset]
        putStrLn $ "    expected type checking to " ++ (if expect == ExitSuccess then "pass, but it failed!" else "fail, but it passed!")
        putStrLn "    source program:"
        putStrLn (unlines . map ("    " ++) $ lines source)
        putStrLn "    type checker standard output:"
        putStrLn (unlines . map ("    " ++) $ lines output)
        return score

matches :: ExitCode -> ExitCode -> Bool
matches ExitSuccess ExitSuccess = True
matches (ExitFailure _) (ExitFailure _) = True
matches _ _ = False
