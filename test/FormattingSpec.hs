module FormattingSpec (spec) where
import qualified Test.Hspec as HS (Spec, describe, hspec, it, shouldBe)
import qualified Formatting as F (formatTable)
import qualified SoccerTable as ST (TableEntry(..))

spec :: HS.Spec
spec = do
  HS.describe "whatever" $ do
    HS.it "adds two numbers" $ do
      1 + 2 `HS.shouldBe` 3
