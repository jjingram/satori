import Eval
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "quote" $ do
      it "can evaluate an integer" $ do eval "0" `shouldReturn` (Right 0)
    describe "binary operations" $ do
      it "can add two numbers" $ do eval "(add 1 2)" `shouldReturn` (Right 3)
    describe "lambda" $ do
      it "can take an argument" $ do
        eval "((lambda (x) x) 0)" `shouldReturn` (Right 0)
      it "can take multiple arguments" $ do
        eval "((lambda (x y) x) 0 1)" `shouldReturn` (Right 0)
    describe "let" $ do
      it "can bind local variables and return them" $ do
        eval "(let ((x 0)) x)" `shouldReturn` (Right 0)
