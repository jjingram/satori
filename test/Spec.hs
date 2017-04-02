import Eval
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "quote" $
      it "can represent an integer" $ eval "0" `shouldReturn` Right 0
    describe "binary operations" $
      it "can add two numbers" $ eval "(add 1 2)" `shouldReturn` Right 3
    describe "lambda" $ do
      it "can take an argument" $
        eval "((lambda (x) x) 0)" `shouldReturn` Right 0
      it "can take multiple arguments" $
        eval "((lambda (x y) x) 0 1)" `shouldReturn` Right 0
      it "can take a function as an argument" $
        eval "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldReturn` Right 0
      it "can be nested and use variables in scope" $
        eval "((lambda (x) ((lambda (y) x) 1)) 0)" `shouldReturn` Right 0
      it "can return functions" $
        eval "(((lambda (x) (lambda (y) x)) 0) 1)" `shouldReturn` Right 0
    describe "let" $ do
      it "can bind local variables and return them" $
        eval "(let ((x 0)) x)" `shouldReturn` Right 0
      it "can bind functions and evaluate them" $
        eval "(let ((f (lambda (x) x))) (f 0))" `shouldReturn` Right 0
      it "can bind variables to variables" $
        eval "((lambda (x) (let ((y x)) y)) 0)" `shouldReturn` Right 0
      it "can bind variables sequentially" $
        eval "((lambda (x) (let ((y x) (z y)) z)) 0)" `shouldReturn` Right 0
    describe "define" $ do
      it "can define constants" $
        eval "(define (x) 0) (x)" `shouldReturn` Right 0
      it "can define functions" $
        eval "(define (id x) x) (id 0)" `shouldReturn` Right 0
      it "can define recursive functions" $
        eval
          "(define (factorial n) (if (eq n 0) 1 (mul n (factorial (sub n 1))))) (factorial 5)" `shouldReturn`
        Right 120
      it "can define mutually recursive functions" $
        eval
          "(define (even? n) (if (eq n 0) 1 (odd? (sub n 1)))) (define (odd? n) (if (eq n 0) 0 (even? (sub n 1)))) (even? 5)" `shouldReturn`
        Right 0
    describe "if" $ do
      it "can choose the true branch" $
        eval "(if (eq 0 0) 0 1)" `shouldReturn` Right 0
      it "can choose the false branch" $
        eval "(if (eq 0 1) 0 1)" `shouldReturn` Right 1
