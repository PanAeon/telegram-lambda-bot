module LambdaSpec where

import           Lambda
import           Test.Hspec

spec :: Spec
spec = do
  describe "lambda" $ do
      describe "parser" $  do
        it "should parse simple expr"  $
          parseExpression "\\f.\\x.f (f ((\\x.x) x))" `shouldBe` Right (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (Var 'f') (App (Lambda 'x' (Var 'x')) (Var 'x'))))))

      describe "beta (normal form reduction)" $ do
        it "should correctly reduce simple expr" $
          (beta $ parseOrFail "(\\a.\\b.a) c ((\\d.e) d)") `shouldBe` Var 'c'
        it "should parse" $
          (beta $ parseOrFail "(\\x.a)((\\x.x x)(\\y.y y))") `shouldBe` Var 'a'
        it "should eval" $
          (beta $ parseOrFail "((\\x.\\x.y) c (\\z.(\\w.(\\b.(\\a.a) c) z) f))") `shouldBe` Var 'y'
        it "should eval" $
          (beta $ parseOrFail "((\\a.x) ((\\a.a a)(\\a.((\\b.a b) a))))" ) `shouldBe` Var 'x'
