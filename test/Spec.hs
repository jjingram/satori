import Eval
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "quote" $ do
      it "can represent an integer" $ do eval "0" `shouldReturn` (Right 0)
    describe "binary operations" $ do
      it "can add two numbers" $ do eval "(add 1 2)" `shouldReturn` (Right 3)
    describe "lambda" $ do
      it "can take an argument" $ do
        eval "((lambda (x) x) 0)" `shouldReturn` (Right 0)
      it "can take multiple arguments" $ do
        eval "((lambda (x y) x) 0 1)" `shouldReturn` (Right 0)
      it "can take a function as an argument" $ do
        eval "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldReturn` (Right 0)
      it "can be nested and capture variables in scope" $ do
        eval "((lambda (x) ((lambda (y) x) 1)) 0)" `shouldReturn` (Right 0)
      it "can return functions" $ do pendingWith "`y` is a type variable, FIXME"
        --eval "((lambda (x) (lambda (y) x)) 0)" `shouldReturn` (Right 0)
    describe "let" $ do
      it "can bind local variables and return them" $ do
        eval "(let ((x 0)) x)" `shouldReturn` (Right 0)
      it "can bind functions and evaluate them" $ do
        eval "(let ((f (lambda (x) x))) (f 0))" `shouldReturn` (Right 0)
      it "can bind variables to variables and return them" $ do
        pendingWith "`let` isn't properly implemented yet"
        --eval "((lambda (x) (let ((y x)) y)) 0)" `shouldReturn` (Right 0)
