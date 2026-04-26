module Main where
import SoccerTable as ST (greet)

main :: IO ()
main =
  print $ ST.greet "Soccer Table"
