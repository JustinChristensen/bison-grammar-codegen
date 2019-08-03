module Bison.Grammar.LexerSpec where

import Test.Hspec
import Bison.Grammar.Lexer

spec :: Spec
spec = do
    describe "foo" $ do
        it "does stuff" $ do
            "foo" `shouldBe` "bar"

