import Test.Hspec

import qualified LoggingSpec
import qualified ShapesSpec
import qualified StrintegerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Logging"      LoggingSpec.spec
  describe "Shapes"       ShapesSpec.spec
  describe "Strinteger"   StrintegerSpec.spec
