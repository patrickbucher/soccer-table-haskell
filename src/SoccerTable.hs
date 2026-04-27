module SoccerTable (GameResult,parse,TableEntry,fromGameResult) where
import Text.Regex.Posix ((=~~))

data GameResult = GameResult
  { homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  }
  deriving (Show)

parse :: String -> Maybe GameResult
parse result =
  let
    matches :: Maybe (String,String,String,[String])
    matches = result =~~ "^(.+) ([0-9]+):([0-9]+) (.+)$"
  in
    case matches of
      Just (_, _, _, [ht, hg, ag, at]) -> Just GameResult
        { homeTeam = ht
        , awayTeam = at
        , homeGoals = read hg :: Int
        , awayGoals = read ag :: Int
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
  deriving (Show)

fromGameResult :: GameResult -> (TableEntry,TableEntry)
fromGameResult result =
  let
    (hG,aG) = (homeGoals result,awayGoals result)
    (hP,aP,hW,aW,hT,aT,hL,aL) =
      case (compare hG aG) of
        LT -> (0,3,0,1,0,0,1,0)
        EQ -> (1,1,0,0,1,1,0,0)
        GT -> (3,0,1,0,0,0,0,1)
  in
    (TableEntry
      {
        rank = 0
      , name = (homeTeam result)
      , points = hP
      , won = hW
      , tied = hT
      , lost = hL
      , scored = hG 
      , conceded = aG 
      , difference = hG - aG 
      },
    TableEntry
      { 
        rank = 0
      , name = (homeTeam result)
      , points = aP
      , won = aW
      , tied = aT
      , lost = aL
      , scored = aG
      , conceded = hG
      , difference = aG - hG
      })
