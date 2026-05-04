module Misc (padLeft, padRight)
where

padLeft :: String -> Int -> String
padLeft s n =
  let
    l = length s
    m = n - l
  in
    take m (repeat ' ') <> s

padRight :: String -> Int -> String
padRight s n =
  let
    l = length s
    m = n - l
  in
    s <> take m (repeat ' ')
