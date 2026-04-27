module SoccerTable (GameResult, parse) where
import Text.Regex.Base
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
    pattern = "^(.+) ([0-9]+):([0-9]+) (.+)$"
  in
    case result =~~ pattern of
      Just (_, _, _, [homeTeam, homeGoals, awayGoals, awayTeam]) -> Just GameResult
        { homeTeam = homeTeam
        , awayTeam = awayTeam
        , homeGoals = read homeGoals :: Int
        , awayGoals = read awayGoals :: Int
        }
      _ -> Nothing
