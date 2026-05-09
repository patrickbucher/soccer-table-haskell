module Main (main) where
import qualified Test.Hspec as HS (Spec, describe, hspec)
import qualified FormattingSpec
import qualified SoccerTableSpec

main :: IO ()
main = HS.hspec spec

spec :: HS.Spec
spec = do
  HS.describe "Formatting" FormattingSpec.spec
  HS.describe "SoccerTable" SoccerTableSpec.spec
