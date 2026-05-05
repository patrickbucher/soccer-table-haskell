module Formatting (formatTable)
where
import qualified SoccerTable as ST (TableEntry(..))

padLeft :: String -> Int -> String
padLeft s n = take (n - length s) (repeat ' ') <> s

padRight :: String -> Int -> String
padRight s n = s <> take (n - length s) (repeat ' ')

data Align = L | R

formatRow :: [(String, Align, Int)] -> String
formatRow cols = interpose (map align cols) " "
  where
    align (s, L, n) = padRight s n
    align (s, R, n) = padLeft s n

interpose :: [String] -> String -> String
interpose ss s = concat $ map (\(l, r) -> l <> r) $ zip ss (repeat s)

titleRow :: String
titleRow =
  formatRow
  [ ("#", R, 3)
  , ("Team", L, 32)
  , ("P", R, 2)
  , ("W", R, 2)
  , ("T", R, 2)
  , ("L", R, 2)
  , ("+", R, 2)
  , ("-", R, 2)
  , ("=", R, 3)
  ]

separatorRow :: String
separatorRow = take 58 (repeat '-')

formatTableEntry :: ST.TableEntry -> String
formatTableEntry e =
  formatRow
  [ ((show $ ST.rank e), R, 3)
  , ((ST.name e), L, 32)
  , ((show $ ST.points e), R, 2)
  , ((show $ ST.won e), R, 2)
  , ((show $ ST.tied e), R, 2)
  , ((show $ ST.lost e), R, 2)
  , ((show $ ST.scored e), R, 2)
  , ((show $ ST.conceded e), R, 2)
  , ((show $ ST.difference e), R, 3)
  ]

formatTable :: [ST.TableEntry] -> String
formatTable t = 
  let
    rows = titleRow : separatorRow : map formatTableEntry t
  in
    interpose rows "\n"
