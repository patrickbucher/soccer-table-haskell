module Main where
import qualified System.Environment as SE (getArgs)

main :: IO ()
main =
  do
    args <- SE.getArgs
    let
      dir = 
        if null args
        then error "usage: soccer-table DIR"
        else (head args)
    putStrLn dir
