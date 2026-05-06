module Main where
import qualified Data.List.Split as S (splitOn)
import qualified Formatting as F (formatTable)
import qualified SoccerTable as ST (calculateTable)
import qualified System.Environment as Env (getArgs)
import qualified System.Exit as Ex (exitWith, ExitCode(..))
import qualified System.IO as IO (readFile)
import qualified System.Directory.Extra as Dir (listFiles)

slurp :: FilePath -> IO [String]
slurp f = do
  c <- IO.readFile f
  return $ S.splitOn "\n" c

main :: IO ()
main = do
  args <- Env.getArgs
  path <- case (firstArg args) of
    Just s -> return s
    _ -> Ex.exitWith (Ex.ExitFailure 1)
  files <- Dir.listFiles path
  results <- mapM slurp files
  let table = ST.calculateTable $ concat results
  let output = F.formatTable table
  putStrLn output
  where
    firstArg [] = Nothing
    firstArg (x:_) = Just x
