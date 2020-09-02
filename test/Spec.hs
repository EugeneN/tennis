import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Text             as T

import           Lib                   (readInput, processInput, formatOutput)


main :: IO ()
main = hspec $ do
    describe "Must match predefined output" $ do
        it "matches line by line (stripped from trailing whitespaces)" $ do
            let inFn     = "data/input.txt"
                sampleFn = "data/output.txt"

            inp    <- readInput inFn 
            sample <- T.lines <$> readInput sampleFn

            let outp = formatOutput . processInput $ inp

            (fmap T.strip outp) `shouldBe`  (fmap T.strip sample)




