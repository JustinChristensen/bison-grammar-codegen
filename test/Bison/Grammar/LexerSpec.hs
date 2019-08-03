{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.LexerSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Bison.Grammar.Lexer
import Helper

spec :: Spec
spec = do
    describe "tokens" $ do
        describe "percentPercent" $ do
            it "should accept \"%%\" and bump the section counter" $ do
                parser percentPercent `shouldSucceedOn` "%%"
                parser percentPercent `shouldFailOn` "%foo"
            it "should be atomic" $ pending
        describe "prologue" $ do
            it "should accept \"%{...%}\"" $ pending
            it "should be atomic" $ pending
        describe "predicate" $ do
            it "should accept \"%?{...}\"" $ pending
            it "should be atomic" $ pending
        describe "pBinary" $ do
            it "should accept \"%binary \"" $ pending
            it "should be atomic" $ pending
        describe "pCode" $ do
            it "should accept \"%code \"" $ pending
            it "should be atomic" $ pending
        describe "pDefaultPrec" $ do
            it "should accept \"%default[-_]prec \"" $ pending
            it "should be atomic" $ pending
        describe "pDefines" $ do
            it "should accept \"%defines \"" $ pending
            it "should be atomic" $ pending
        describe "pDefine" $ do
            it "should accept \"%define \"" $ pending
            it "should be atomic" $ pending
        describe "pDestructor" $ do
            it "should accept \"%destructor \"" $ pending
            it "should be atomic" $ pending
        describe "pDprec" $ do
            it "should accept \"%dprec \"" $ pending
            it "should be atomic" $ pending
        describe "pEmpty" $ do
            it "should accept \"%empty \"" $ pending
            it "should be atomic" $ pending
        describe "pErrorVerbose" $ do
            it "should accept \"%error[-_]verbose \"" $ pending
            it "should be atomic" $ pending
        describe "pExpect" $ do
            it "should accept \"%expect \"" $ pending
            it "should be atomic" $ pending
        describe "pExpectRr" $ do
            it "should accept \"%expect[-_]rr \"" $ pending
            it "should be atomic" $ pending
        describe "pFilePrefix" $ do
            it "should accept \"%file-prefix(\\s*=)? \"" $ pending
            it "should be atomic" $ pending
        describe "pInitialAction" $ do
            it "should accept \"%initial-action \"" $ pending
            it "should be atomic" $ pending
        describe "pGlrParser" $ do
            it "should accept \"%glr-parser \"" $ pending
            it "should be atomic" $ pending
        describe "pLanguage" $ do
            it "should accept \"%language \"" $ pending
            it "should be atomic" $ pending
        describe "pLeft" $ do
            it "should accept \"%left \"" $ pending
            it "should be atomic" $ pending
        describe "pFlag" $ do
            it "should accept \"%(debug|locations) \"" $ pending
            it "should be atomic" $ pending
        describe "pMerge" $ do
            it "should accept \"%merge \"" $ pending
            it "should be atomic" $ pending
        describe "pNamePrefix" $ do
            it "should accept \"%name[-_]prefix(\\s*=)? \"" $ pending
            it "should be atomic" $ pending
        describe "pNoDefaultPrec" $ do
            it "should accept \"%no[-_]default[-_]prec \"" $ pending
            it "should be atomic" $ pending
        describe "pNoLines" $ do
            it "should accept \"%no[-_]lines \"" $ pending
            it "should be atomic" $ pending
        describe "pNonAssoc" $ do
            it "should accept \"%(binary|nonassoc) \"" $ pending
            it "should be atomic" $ pending
        describe "pNonDeterministicParser" $ do
            it "should accept \"%(binary|nonassoc) \"" $ pending
            it "should be atomic" $ pending
        describe "pNterm" $ do
            it "should accept \"%nterm \"" $ pending
            it "should be atomic" $ pending
        describe "pOutput" $ do
            it "should accept \"%output(\\s*=)? \"" $ pending
            it "should be atomic" $ pending
        describe "pFixedOutputFiles" $ do
            it "should accept \"%fixed[-_]output[-_]files \"" $ pending
            it "should be atomic" $ pending
        describe "pParam" $ do
            it "should accept \"%(param|parse-param|lex-param) \"" $ pending
            it "should be atomic" $ pending
        describe "pPrecedence" $ do
            it "should accept \"%precedence \"" $ pending
            it "should be atomic" $ pending
        describe "pPrec" $ do
            it "should accept \"%prec \"" $ pending
            it "should be atomic" $ pending
        describe "pPrinter" $ do
            it "should accept \"%printer \"" $ pending
            it "should be atomic" $ pending
        describe "pPureParser" $ do
            it "should accept \"%pure[-_]parser \"" $ pending
            it "should be atomic" $ pending
        describe "pRequire" $ do
            it "should accept \"%require \"" $ pending
            it "should be atomic" $ pending
        describe "pRight" $ do
            it "should accept \"%right \"" $ pending
            it "should be atomic" $ pending
        describe "pSkeleton" $ do
            it "should accept \"%skeleton \"" $ pending
            it "should be atomic" $ pending
        describe "pStart" $ do
            it "should accept \"%start \"" $ pending
            it "should be atomic" $ pending
        describe "pTokenTable" $ do
            it "should accept \"%token[-_]table \"" $ pending
            it "should be atomic" $ pending
        describe "pToken" $ do
            it "should accept \"%token \"" $ pending
            it "should be atomic" $ pending
        describe "pType" $ do
            it "should accept \"%type \"" $ pending
            it "should be atomic" $ pending
        describe "pUnion" $ do
            it "should accept \"%union \"" $ pending
            it "should be atomic" $ pending
        describe "pVerbose" $ do
            it "should accept \"%verbose \"" $ pending
            it "should be atomic" $ pending
        describe "pYacc" $ do
            it "should accept \"%yacc \"" $ pending
            it "should be atomic" $ pending
        describe "colon" $ do
            it "should accept \":\"" $ pending
            it "should be atomic" $ pending
        describe "equal" $ do
            it "should accept \"=\"" $ pending
            it "should be atomic" $ pending
        describe "pipe" $ do
            it "should accept \"|\"" $ pending
            it "should be atomic" $ pending
        describe "semicolon" $ do
            it "should accept \";\"" $ pending
            it "should be atomic" $ pending
        describe "identifier" $ do
            it "should accept \"FooBar \"" $ pending
            it "should accept \"_fooBar9 \"" $ pending
            it "should not accept \"9fooBar \"" $ pending
            it "should not accept \"fooBar: \"" $ pending
            it "should not accept \"fooBar[ \"" $ pending
            it "should be atomic" $ pending
        describe "idColon" $ do
            it "should accept \"foobar: \"" $ pending
            it "should not consume trailing colon" $ pending
            it "should be atomic" $ pending
        describe "bracketedId" $ do
            it "should accept \"[bar]\"" $ pending
            it "should accept \"_foo[_bar]\"" $ pending
            it "should not accept \"_foo[0_bar]\"" $ pending
            it "should be atomic" $ pending
        describe "integer_" $ do
            it "should accept \"0[xX]A1F9\"" $ pending
            it "should accept \"0314\"" $ pending
            it "should not accept \"1AF\"" $ pending
            it "should not accept \"19x\"" $ pending
            it "should be atomic" $ pending
        describe "character_" $ do
            it "should accept \"'c'\"" $ pending
            it "should accept \"'0'\"" $ pending
            it "should accept \"'\\t'\"" $ pending
            it "should accept \"'\\n'\"" $ pending
            it "should be atomic" $ pending
        describe "string_" $ do
            it "should accept \"\"foo \\t 0314 bar\"\"" $ pending
            it "should be atomic" $ pending
        describe "bracedCode" $ do
            it "should accept \"{...}\"" $ pending
            it "should not match braces in character literals" $ pending
            it "should not match braces in string literals" $ pending
            it "should not match braces in line comments" $ pending
            it "should not match braces in block comments" $ pending
            it "should not match braces in macro definitions" $ pending
            it "should choke on unmatched braces" $ pending
            it "should be atomic" $ pending
        describe "tagAny" $ do
            it "should accept \"<*>\"" $ pending
            it "should be atomic" $ pending
        describe "tagNone" $ do
            it "should accept \"<>\"" $ pending
            it "should be atomic" $ pending
        describe "tag" $ do
            it "should accept \"<foo<bar> -> baz>\"" $ pending
            it "should choke on unmatched angle brackets" $ pending
            it "should be atomic" $ pending
        describe "epilogue" $ do
            it "consume the rest of the input" $ pending
    describe "token" $ do
        it "should take the longest match based on order" $ pending
