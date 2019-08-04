{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.LexerSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Bison.Grammar.Types
import Bison.Grammar.Lexer
import Helper
import Fixtures

spec :: Spec
spec = do
    describe "percentPercent_" $ do
        it "should accept \"%%\"" $ do
            parser percentPercent_`shouldSucceedOn` "%%"
            parser percentPercent_"%%" `parseSatisfies` (== PERCENT_PERCENT)
            parser percentPercent_ `shouldFailOn` "%foo"
        it "should be atomic" $ do
            percentPercent_ `isAtomicGiven` "%foo"
    describe "prologue_" $ do
        it "should accept \"%{...%}\"" $ do
            input <- prologueFixture
            parser prologue_ `shouldSucceedOn` input
            parser prologue_ input `parseSatisfies` (\x -> case x of PROLOGUE _ -> True; _ -> False)
        it "should terminate given \"%{{}%}}\"" $ do
            parser prologue_ `shouldSucceedOn` "%{{}%}"
        it "should be atomic" $ do
            prologue_ `isAtomicGiven` "{ // {\n {} "
    describe "predicate_" $ do
        it "should accept \"%?{...}\"" $ do
            let input = "%?{ foo->is_true }"
            parser predicate_ `shouldSucceedOn` input
            parser predicate_ input `parseSatisfies` (\x -> case x of BRACED_PREDICATE _ -> True; _ -> False)
        it "should be atomic" $ do
            predicate_ `isAtomicGiven` "%?{fo{}o"
    describe "pCode_" $ do
        it "should accept \"%code \"" $ do
            parser pCode_ "%code " `parseSatisfies` (== PERCENT_CODE)
            parser pCode_ `shouldFailOn` "%codea"
        it "should be atomic" $ do
            pCode_ `isAtomicGiven` "%codea"
    describe "pDefaultPrec_" $ do
        it "should accept \"%default[-_]prec \"" $ do
            parser pDefaultPrec_ "%default-prec " `parseSatisfies` (== PERCENT_DEFAULT_PREC)
            parser pDefaultPrec_ "%default_prec " `parseSatisfies` (== PERCENT_DEFAULT_PREC)
            parser pDefaultPrec_ `shouldFailOn` "%default-preca"
        it "should be atomic" $ do
            pDefaultPrec_ `isAtomicGiven` "%default-pre"
    describe "pDefines_" $ do
        it "should accept \"%defines \"" $ do
            parser pDefines_ "%defines" `parseSatisfies` (== PERCENT_DEFINES)
            parser pDefines_ `shouldFailOn` "%definea"
            parser pDefines_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pDefines_ `isAtomicGiven` "%define"
    describe "pDefine_" $ do
        it "should accept \"%define \"" $ pending
        it "should be atomic" $ do
            pDefine_ `isAtomicGiven` "%defin"
    describe "pDestructor_" $ do
        it "should accept \"%destructor \"" $ pending
        it "should be atomic" $ do
            pDestructor_ `isAtomicGiven` "%destruct"
    describe "pDprec_" $ do
        it "should accept \"%dprec \"" $ pending
        it "should be atomic" $ do
            pDprec_ `isAtomicGiven` "%dpre"
    describe "pEmpty_" $ do
        it "should accept \"%empty \"" $ pending
        it "should be atomic" $ do
            pEmpty_ `isAtomicGiven` "%emp"
    describe "pErrorVerbose_" $ do
        it "should accept \"%error[-_]verbose \"" $ pending
        it "should be atomic" $ do
            pErrorVerbose_ `isAtomicGiven` "%error-"
    describe "pExpect_" $ do
        it "should accept \"%expect \"" $ pending
        it "should be atomic" $ do
            pExpect_ `isAtomicGiven` "%expec"
    describe "pExpectRr_" $ do
        it "should accept \"%expect[-_]rr \"" $ pending
        it "should be atomic" $ do
            pExpectRr_ `isAtomicGiven` "%expect_r"
    describe "pFilePrefix_" $ do
        it "should accept \"%file-prefix(\\s*=)? \"" $ pending
        it "should be atomic" $ do
            pFilePrefix_ `isAtomicGiven` "%file"
    describe "pInitialAction_" $ do
        it "should accept \"%initial-action \"" $ pending
        it "should be atomic" $ do
            pInitialAction_ `isAtomicGiven` "%init"
    describe "pGlrParser_" $ do
        it "should accept \"%glr-parser \"" $ pending
        it "should be atomic" $ do
            pGlrParser_ `isAtomicGiven` "%glr-parseraf:"
    describe "pLanguage_" $ do
        it "should accept \"%language \"" $ pending
        it "should be atomic" $ do
            pLanguage_ `isAtomicGiven` "%lang"
    describe "pLeft_" $ do
        it "should accept \"%left \"" $ pending
        it "should be atomic" $ do
            pLeft_ `isAtomicGiven` "%lefter"
    describe "pFlag_" $ do
        it "should accept \"%(debug|locations) \"" $ pending
        it "should be atomic" $ do
            pFlag_ `isAtomicGiven` "%locat"
    describe "pMerge_" $ do
        it "should accept \"%merge \"" $ pending
        it "should be atomic" $ do
            pMerge_ `isAtomicGiven` "%merg"
    describe "pNamePrefix_" $ do
        it "should accept \"%name[-_]prefix(\\s*=)? \"" $ pending
        it "should be atomic" $ do
            pNamePrefix_ `isAtomicGiven` "%name_-"
    describe "pNoDefaultPrec_" $ do
        it "should accept \"%no[-_]default[-_]prec \"" $ pending
        it "should be atomic" $ do
            pNoDefaultPrec_ `isAtomicGiven` "%no-default_pr"
    describe "pNoLines_" $ do
        it "should accept \"%no[-_]lines \"" $ pending
        it "should be atomic" $ do
            pNoLines_ `isAtomicGiven` "%nolines"
    describe "pNonAssoc_" $ do
        it "should accept \"%(binary|nonassoc) \"" $ pending
        it "should be atomic" $ do
            pNonAssoc_ `isAtomicGiven` "%nonassocT"
    describe "pNonDeterministicParser_" $ do
        it "should accept \"%(binary|nonassoc) \"" $ pending
        it "should be atomic" $ do
            pNonDeterministicParser_ `isAtomicGiven` "ndp"
    describe "pNterm_" $ do
        it "should accept \"%nterm \"" $ pending
        it "should be atomic" $ do
            pNterm_ `isAtomicGiven` "%nt?"
    describe "pOutput_" $ do
        it "should accept \"%output(\\s*=)? \"" $ pending
        it "should be atomic" $ do
            pOutput_ `isAtomicGiven` "%outpuT"
    describe "pFixedOutputFiles_" $ do
        it "should accept \"%fixed[-_]output[-_]files \"" $ pending
        it "should be atomic" $ do
            pFixedOutputFiles_ `isAtomicGiven` "%fixed_output"
    describe "pParam_" $ do
        it "should accept \"%(param|parse-param|lex-param) \"" $ pending
        it "should be atomic" $ do
            pParam_ `isAtomicGiven` "%lex-parm"
    describe "pPrecedence_" $ do
        it "should accept \"%precedence \"" $ pending
        it "should be atomic" $ do
            pPrecedence_ `isAtomicGiven` "%prec"
    describe "pPrec_" $ do
        it "should accept \"%prec \"" $ pending
        it "should be atomic" $ do
            pPrec_ `isAtomicGiven` "%precedence"
    describe "pPrinter_" $ do
        it "should accept \"%printer \"" $ pending
        it "should be atomic" $ do
            pPrinter_ `isAtomicGiven` "%print"
    describe "pPureParser_" $ do
        it "should accept \"%pure[-_]parser \"" $ pending
        it "should be atomic" $ do
            pPureParser_ `isAtomicGiven` "%pparser"
    describe "pRequire_" $ do
        it "should accept \"%require \"" $ pending
        it "should be atomic" $ do
            pRequire_ `isAtomicGiven` "%req"
    describe "pRight_" $ do
        it "should accept \"%right \"" $ pending
        it "should be atomic" $ do
            pRight_ `isAtomicGiven` "rt"
    describe "pSkeleton_" $ do
        it "should accept \"%skeleton \"" $ pending
        it "should be atomic" $ do
            pSkeleton_ `isAtomicGiven` "%skelly"
    describe "pStart_" $ do
        it "should accept \"%start \"" $ pending
        it "should be atomic" $ do
            pStart_ `isAtomicGiven` "%tart"
    describe "pTokenTable_" $ do
        it "should accept \"%token[-_]table \"" $ pending
        it "should be atomic" $ do
            pTokenTable_ `isAtomicGiven` "%token-"
    describe "pToken_" $ do
        it "should accept \"%token \"" $ pending
        it "should be atomic" $ do
            pToken_ `isAtomicGiven` "%to9ky"
    describe "pType_" $ do
        it "should accept \"%type \"" $ pending
        it "should be atomic" $ do
            pType_ `isAtomicGiven` "%typer"
    describe "pUnion_" $ do
        it "should accept \"%union \"" $ pending
        it "should be atomic" $ do
            pUnion_ `isAtomicGiven` "%unio!"
    describe "pVerbose_" $ do
        it "should accept \"%verbose \"" $ pending
        it "should be atomic" $ do
            pVerbose_ `isAtomicGiven` "%vb"
    describe "pYacc_" $ do
        it "should accept \"%yacc \"" $ pending
        it "should be atomic" $ do
            pYacc_ `isAtomicGiven` "%yc"
    describe "colon_" $ do
        it "should accept \":\"" $ pending
        it "should be atomic" $ do
            colon_ `isAtomicGiven` "!"
    describe "equal_" $ do
        it "should accept \"=\"" $ pending
        it "should be atomic" $ do
            equal_ `isAtomicGiven` "!"
    describe "pipe_" $ do
        it "should accept \"|\"" $ pending
        it "should be atomic" $ do
            pipe_ `isAtomicGiven` "!"
    describe "semicolon_" $ do
        it "should accept \";\"" $ pending
        it "should be atomic" $ do
            semicolon_ `isAtomicGiven` "!"
    describe "identifier_" $ do
        it "should accept \"FooBar \"" $ pending
        it "should accept \"_fooBar9 \"" $ pending
        it "should not accept \"9fooBar \"" $ pending
        it "should not accept \"fooBar: \"" $ pending
        it "should not accept \"fooBar[ \"" $ pending
        it "should be atomic" $ do
            identifier_ `isAtomicGiven` "foobar:"
    describe "idColon_" $ do
        it "should accept \"foobar: \"" $ pending
        it "should not consume trailing colon" $ pending
        it "should be atomic" $ do
            idColon_ `isAtomicGiven` "foobar"
    describe "bracketedId_" $ do
        it "should accept \"[bar]\"" $ pending
        it "should accept \"_foo[_bar]\"" $ pending
        it "should not accept \"_foo[0_bar]\"" $ pending
        it "should be atomic" $ do
            bracketedId_ `isAtomicGiven` "foo[ba"
    describe "integer_" $ do
        it "should accept \"0[xX]A1F9\"" $ pending
        it "should accept \"0314\"" $ pending
        it "should not accept \"1AF\"" $ pending
        it "should not accept \"19x\"" $ pending
        it "should be atomic" $ do
            integer_ `isAtomicGiven` "1Ax"
    describe "character_" $ do
        it "should accept \"'c'\"" $ pending
        it "should accept \"'0'\"" $ pending
        it "should accept \"'\\t'\"" $ pending
        it "should accept \"'\\n'\"" $ pending
        it "should be atomic" $ do
            character_ `isAtomicGiven` "'a"
    describe "string_" $ do
        it "should accept \"\"foo \\t 0314 bar\"\"" $ pending
        it "should be atomic" $ do
            string_ `isAtomicGiven` "\"foo\\t"
    describe "bracedCode_" $ do
        it "should accept \"{...}\"" $ pending
        it "should not match braces in character literals" $ pending
        it "should not match braces in string literals" $ pending
        it "should not match braces in line comments" $ pending
        it "should not match braces in block comments" $ pending
        it "should not match braces in macro definitions" $ pending
        it "should choke on unmatched braces" $ pending
        it "should be atomic" $ do
            bracedCode_ `isAtomicGiven` "{{ //foo }\n int foo() {} bar"
    describe "tagAny_" $ do
        it "should accept \"<*>\"" $ pending
        it "should be atomic" $ do
            tagAny_ `isAtomicGiven` "<*"
    describe "tagNone_" $ do
        it "should accept \"<>\"" $ pending
        it "should be atomic" $ do
            tagNone_ `isAtomicGiven` ">"
    describe "tag_" $ do
        it "should accept \"<foo<bar> -> baz>\"" $ pending
        it "should choke on unmatched angle brackets" $ pending
        it "should be atomic" $ do
            tag_ `isAtomicGiven` "<foo -> bar>>"
    describe "epilogue_" $ do
        it "consume the rest of the input" $ pending
    describe "token" $ do
        it "should take the longest match based on order" $ pending
