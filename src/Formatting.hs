module Formatting (formatTable)
where
import qualified SoccerTable as ST (TableEntry(..))

data Align = L | R

colSpec :: [(String, Align, Int)]
colSpec =
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

formatRow :: [(String, Align, Int)] -> String
formatRow cols = interpose (map align cols) " "
  where
    align (s, L, n) = padRight s n
    align (s, R, n) = padLeft s n

interpose :: [String] -> String -> String
interpose ss s = concat $ map (\(l, r) -> l <> r) $ zip ss (repeat s)

padLeft :: String -> Int -> String
padLeft s n = take (n - length s) (repeat ' ') <> s

padRight :: String -> Int -> String
padRight s n = s <> take (n - length s) (repeat ' ')

titleRow :: String
titleRow = formatRow colSpec

separatorRow :: String
separatorRow =
  let
    c = sum $ map (\(_, _, n) -> n) colSpec
    s = (length colSpec) - 1
  in
    take (c + s) $ repeat '-'

formatTableEntry :: ST.TableEntry -> String
formatTableEntry e =
  let
    fields =
      [ show $ ST.rank e
      , ST.name e
      , show $ ST.points e
      , show $ ST.won e
      , show $ ST.tied e
      , show $ ST.lost e
      , show $ ST.scored e
      , show $ ST.conceded e
      , show $ ST.difference e
      ]
    cols = zipWith (\f (_, a, w) -> (f, a, w)) fields colSpec
  in
    formatRow cols

formatTable :: [ST.TableEntry] -> String
formatTable t = 
  let
    rows = titleRow : separatorRow : map formatTableEntry t
  in
    interpose rows "\n"
