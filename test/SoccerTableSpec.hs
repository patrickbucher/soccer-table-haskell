module SoccerTableSpec (spec) where
import qualified Test.Hspec as HS (Spec, describe, it, shouldBe)
import qualified SoccerTable as ST (GameResult(..), TableEntry(..), fromGameResult, fromRawResult)

spec :: HS.Spec
spec = do
  HS.describe "parse result" $ do
    HS.it "parses a raw game result" $ do
      ST.fromRawResult "Foo 3:2 Bar" `HS.shouldBe` Just ST.GameResult
        { ST.homeTeam = "Foo"
        , ST.awayTeam = "Bar"
        , ST.homeGoals = 3
        , ST.awayGoals = 2
        }
    HS.it "won't parse an invalid game result" $ do
      ST.fromRawResult "Hello, World!" `HS.shouldBe` Nothing
    
    HS.describe "create table entry from raw result" $ do
      HS.it "creates entries for win/loose situation" $ do
        let result = ST.GameResult {
            ST.homeTeam = "A"
          , ST.awayTeam = "B"
          , ST.homeGoals = 4
          , ST.awayGoals = 1
          }
        let home = ST.TableEntry {
            ST.rank = 0
          , ST.name = "A"
          , ST.points = 3
          , ST.won = 1
          , ST.tied = 0
          , ST.lost = 0
          , ST.scored = 4
          , ST.conceded = 1
          , ST.difference = 3
          }
        let away = ST.TableEntry {
            ST.rank = 0
          , ST.name = "B"
          , ST.points = 0
          , ST.won = 0
          , ST.tied = 0
          , ST.lost = 1
          , ST.scored = 1
          , ST.conceded = 4
          , ST.difference = (-3)
          }
        ST.fromGameResult result `HS.shouldBe` [home, away]
      HS.it "creates entries for draw situation" $ do
        let result = ST.GameResult {
            ST.homeTeam = "A"
          , ST.awayTeam = "B"
          , ST.homeGoals = 2
          , ST.awayGoals = 2
          }
        let home = ST.TableEntry {
            ST.rank = 0
          , ST.name = "A"
          , ST.points = 1
          , ST.won = 0
          , ST.tied = 1
          , ST.lost = 0
          , ST.scored = 2
          , ST.conceded = 2
          , ST.difference = 0
          }
        let away = ST.TableEntry {
            ST.rank = 0
          , ST.name = "B"
          , ST.points = 1
          , ST.won = 0
          , ST.tied = 1
          , ST.lost = 0
          , ST.scored = 2
          , ST.conceded = 2
          , ST.difference = 0
          }
        ST.fromGameResult result `HS.shouldBe` [home, away]

