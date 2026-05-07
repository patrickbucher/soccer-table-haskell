module Main (main) where
import qualified Test.Hspec as HS (hspec, describe, it, shouldBe)
import qualified SoccerTable as ST (fromRawResult, GameResult(..))

main :: IO ()
main = HS.hspec $ do
  HS.describe "SoccerTable.fromRawResult" $ do
    HS.it "parses a raw game result" $ do
      ST.fromRawResult "Foo 3:2 Bar" `HS.shouldBe` Just ST.GameResult
        { ST.homeTeam = "Foo"
        , ST.awayTeam = "Bar"
        , ST.homeGoals = 3
        , ST.awayGoals = 2
        }
    HS.it "won't parse an invalid game result" $ do
      ST.fromRawResult "Hello, World!" `HS.shouldBe` Nothing
