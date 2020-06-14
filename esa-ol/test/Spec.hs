{-# LANGUAGE QuasiQuotes #-}

import Data.Text as T
import Data.Attoparsec.Text as AT
import Data.Maybe
import Test.Hspec
import Text.RawString.QQ
import OL

fullEval :: Text -> Maybe (Either Text Double)
fullEval t =  AT.maybeResult $ OL.eval <$> AT.parse OL.parseProgram t

main :: IO ()
main = hspec $ do
    describe "Simple.Test" $ do
        it "const" $ do
            fullEval (T.pack "2;") `shouldBe` Just (Right 2)
        it "assign" $ do
            fullEval (T.pack [r|VARA := 2;
            4;|]) `shouldBe` Just (Right 4)
        it "assign eval" $ do
            fullEval (T.pack [r|VARA := 2;
            A;|]) `shouldBe` Just (Right 2)