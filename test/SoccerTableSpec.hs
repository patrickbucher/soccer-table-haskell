module SoccerTableSpec (spec)
where
import qualified Data.Map as M (fromList)
import qualified Test.Hspec as HS (Spec, describe, it, shouldBe)
import qualified SoccerTable as ST (
  GameResult(..),
  TableEntry(..),
  fromGameResult,
  fromRawResult,
  merge,
  mergeAll,
  calculateTable
  )

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

  HS.describe "merge table entries" $ do
    HS.it "merges compatible entries" $ do
      let left = ST.TableEntry {
          ST.rank = 0
        , ST.name = "A"
        , ST.points = 13
        , ST.won = 3
        , ST.tied = 4
        , ST.lost = 2
        , ST.scored = 17
        , ST.conceded = 15
        , ST.difference = 2
        }
      let right = ST.TableEntry {
          ST.rank = 0
        , ST.name = "A"
        , ST.points = 6
        , ST.won = 1
        , ST.tied = 3
        , ST.lost = 5
        , ST.scored = 4
        , ST.conceded = 20
        , ST.difference = (-16)
        }
      let expected = ST.TableEntry {
          ST.rank = 0
        , ST.name = "A"
        , ST.points = 19
        , ST.won = 4
        , ST.tied = 7
        , ST.lost = 7
        , ST.scored = 21
        , ST.conceded = 35
        , ST.difference = (-14)
      }
      let actual = ST.merge left right
      actual `HS.shouldBe` Just expected
    HS.it "merges incompatible entries" $ do
      let left = ST.TableEntry {
          ST.rank = 0
        , ST.name = "X"
        , ST.points = 13
        , ST.won = 3
        , ST.tied = 4
        , ST.lost = 2
        , ST.scored = 17
        , ST.conceded = 15
        , ST.difference = 2
        }
      let right = ST.TableEntry {
          ST.rank = 0
        , ST.name = "Y"
        , ST.points = 6
        , ST.won = 1
        , ST.tied = 3
        , ST.lost = 5
        , ST.scored = 4
        , ST.conceded = 20
        , ST.difference = (-16)
        }
      let actual = ST.merge left right
      actual `HS.shouldBe` Nothing

  HS.describe "merges a list of table entries" $ do
    HS.it "merges four entries of two teams" $ do
      let a1 = ST.TableEntry {
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
      let a2 = ST.TableEntry {
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
      let b1 = ST.TableEntry {
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
      let b2 = ST.TableEntry {
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
      let aM = ST.TableEntry {
          ST.rank = 0
        , ST.name = "A"
        , ST.points = 4
        , ST.won = 1
        , ST.tied = 1
        , ST.lost = 0
        , ST.scored = 6
        , ST.conceded = 3
        , ST.difference = 3
        }
      let bM = ST.TableEntry {
          ST.rank = 0
        , ST.name = "B"
        , ST.points = 1
        , ST.won = 0
        , ST.tied = 1
        , ST.lost = 1
        , ST.scored = 3
        , ST.conceded = 6
        , ST.difference = (-3)
        }
      let expected = M.fromList [("A", aM), ("B", bM)]
      let actual = ST.mergeAll [a1, a2, b1, b2]
      actual `HS.shouldBe` expected

  HS.describe "calculate table" $ do
    HS.it "calculates the table for results A 2:0 B, B 1:1 C, C 0:3 A" $ do
      let rawResults = ["A 2:0 B", "B 1:1 C", "C 0:3 A"]
      let a = ST.TableEntry {
          ST.rank = 1
        , ST.name = "A"
        , ST.points = 6
        , ST.won = 2
        , ST.tied = 0
        , ST.lost = 0
        , ST.scored = 5
        , ST.conceded = 0
        , ST.difference = 5
        }
      let b = ST.TableEntry {
          ST.rank = 2
        , ST.name = "B"
        , ST.points = 1
        , ST.won = 0
        , ST.tied = 1
        , ST.lost = 1
        , ST.scored = 1
        , ST.conceded = 3
        , ST.difference = (-2)
        }
      let c = ST.TableEntry {
          ST.rank = 3
        , ST.name = "C"
        , ST.points = 1
        , ST.won = 0
        , ST.tied = 1
        , ST.lost = 1
        , ST.scored = 1
        , ST.conceded = 4
        , ST.difference = (-3)
        }
      let expected = [a, b, c]
      let actual = ST.calculateTable rawResults
      actual `HS.shouldBe` expected
