module Main where
import qualified Data.List.Split as S (splitOn)
import qualified SoccerTable as ST (calculateTable)
import qualified System.Environment as Env (getArgs)
import qualified System.Exit as Ex (exitWith, ExitCode(..))
import qualified System.IO as IO (IOMode(..), hGetContents, openFile)
import qualified System.Directory.Extra as Dir (listFiles)

slurp :: FilePath -> IO [String]
slurp f = do
  h <- IO.openFile f IO.ReadMode
  c <- IO.hGetContents h
  let l = S.splitOn "\n" c
  return l


main :: IO ()
main = do
  args <- Env.getArgs
  path <-
    if null args
    then Ex.exitWith (Ex.ExitFailure 1)
    else return (head args)
  files <- Dir.listFiles path
  results <- mapM slurp files
  let table = ST.calculateTable $ concat results
  print table
