module SoccerTable
( GameResult
, fromRawResult
, TableEntry
, fromGameResult
, merge
, mergeAll
, calculateTable
)
where
import Text.Regex.Posix ((=~~))
import qualified Data.Map as M
import qualified Data.List as L (sort)

data GameResult = GameResult
  { homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  }
  deriving (Show)

fromRawResult :: String -> Maybe GameResult
fromRawResult result =
  let
    matches :: Maybe (String, String, String, [String])
    matches = result =~~ "^(.+) ([0-9]+):([0-9]+) (.+)$"
  in
    case matches of
      Just (_, _, _, [hT, hG, aG, aT]) -> Just GameResult
        { homeTeam = hT
        , awayTeam = aT
        , homeGoals = read hG :: Int
        , awayGoals = read aG :: Int
        }
      _ -> Nothing

data TableEntry = TableEntry
  { rank :: Int
  , name :: String
  , points :: Int
  , won :: Int
  , tied :: Int
  , lost :: Int
  , scored :: Int
  , conceded :: Int
  , difference :: Int
  }
  deriving (Eq, Show)

instance Ord TableEntry where
  compare l r = compare
    (points l, difference l, won l, name r)
    (points r, difference r, won r, name l)

fromGameResult :: GameResult -> [TableEntry]
fromGameResult result =
  let
    (hG, aG) = (homeGoals result, awayGoals result)
    (hP, aP, hW, aW, hT, aT, hL, aL) =
      case (compare hG aG) of
        LT -> (0, 3, 0, 1, 0, 0, 1, 0)
        EQ -> (1, 1, 0, 0, 1, 1, 0, 0)
        GT -> (3, 0, 1, 0, 0, 0, 0, 1)
  in
    [ TableEntry
      { rank = 0
      , name = (homeTeam result)
      , points = hP
      , won = hW
      , tied = hT
      , lost = hL
      , scored = hG 
      , conceded = aG 
      , difference = hG - aG 
      }
    , TableEntry
      { rank = 0
      , name = (awayTeam result)
      , points = aP
      , won = aW
      , tied = aT
      , lost = aL
      , scored = aG
      , conceded = hG
      , difference = aG - hG
      }
    ]

merge :: TableEntry -> TableEntry -> Maybe TableEntry
merge left right =
  if (name left) == (name right)
  then Just TableEntry
  { rank = 0
  , name = (name left)
  , points = (points left) + (points right)
  , won = (won left) + (won right)
  , tied = (tied left) + (tied right)
  , lost = (lost left) + (lost right)
  , scored = (scored left) + (scored right)
  , conceded = (conceded left) + (conceded right)
  , difference = (difference left) + (difference right)
  }
  else Nothing

mergeAll :: [TableEntry] -> (M.Map String TableEntry)
mergeAll entries =
  foldl combine (M.fromList []) entries
  where
    combine :: (M.Map String TableEntry) -> TableEntry -> (M.Map String TableEntry)
    combine acc e =
      case M.lookup (name e) acc of
        Just x ->
          case (merge e x) of
            Just f -> M.insert (name f) f acc
            Nothing -> acc
        Nothing -> M.insert (name e) e acc

calculateTable :: [String] -> [TableEntry]
calculateTable rawResults =
  let
    nested = concat [fromGameResult x | Just x <- map fromRawResult rawResults]
  in
    calcRank . reverse . L.sort . M.elems . mergeAll $ nested
  where
    calcRank = map (\(r, e) -> e { rank = r }) . zip [1..]
