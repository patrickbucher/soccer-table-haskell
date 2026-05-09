module FormattingSpec (spec) where
import qualified Test.Hspec as HS (Spec, describe, it, shouldBe)
import qualified Formatting as F (formatTable)
import qualified SoccerTable as ST (TableEntry(..))

spec :: HS.Spec
spec = do
  HS.describe "formatting" $ do
    HS.it "format table for results A 2:0 B, B 1:1 C, C 0:3 A" $ do
      F.formatTable [a, b, c] `HS.shouldBe` table
      where
        a = ST.TableEntry
          { ST.rank = 1
          , ST.name = "A"
          , ST.points = 7
          , ST.won = 2
          , ST.tied = 0
          , ST.lost = 0
          , ST.scored = 5
          , ST.conceded = 0
          , ST.difference = 5
          }
        b = ST.TableEntry
          { ST.rank = 2
          , ST.name = "B"
          , ST.points = 1
          , ST.won = 0
          , ST.tied = 1
          , ST.lost = 1
          , ST.scored = 1
          , ST.conceded = 3
          , ST.difference = (-2)
          }
        c = ST.TableEntry
          { ST.rank = 3
          , ST.name = "C"
          , ST.points = 1
          , ST.won = 0
          , ST.tied = 1
          , ST.lost = 1
          , ST.scored = 1
          , ST.conceded = 4
          , ST.difference = (-3)
          }
        table = "\
\  # Team                              P  W  T  L  +  -   = \n\
\----------------------------------------------------------\n\
\  1 A                                 7  2  0  0  5  0   5 \n\
\  2 B                                 1  0  1  1  1  3  -2 \n\
\  3 C                                 1  0  1  1  1  4  -3 \n"
