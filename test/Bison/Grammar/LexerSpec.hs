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
            parser prologue_ input `parseSatisfies` isPrologue
        it "should terminate given \"%{{}%}}\"" $ do
            parser prologue_ `shouldSucceedOn` "%{{}%}"
        it "should be atomic" $ do
            prologue_ `isAtomicGiven` "{ // {\n {} "
    describe "predicate_" $ do
        it "should accept \"%?{...}\"" $ do
            let input = "%?{ foo->is_true }"
            parser predicate_ `shouldSucceedOn` input
            parser predicate_ input `parseSatisfies` isBracedPredicate
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
        it "should accept \"%define \"" $ do
            parser pDefine_ "%define // foo\n" `parseSatisfies` (== PERCENT_DEFINE)
            parser pDefine_ `shouldFailOn` "%definea"
            parser pDefine_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pDefine_ `isAtomicGiven` "%defin"
    describe "pDestructor_" $ do
        it "should accept \"%destructor \"" $ do
            parser pDestructor_ "%destructor /* */" `parseSatisfies` (== PERCENT_DESTRUCTOR)
            parser pDestructor_ `shouldFailOn` "%destructora"
            parser pDestructor_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pDestructor_ `isAtomicGiven` "%destruct"
    describe "pDprec_" $ do
        it "should accept \"%dprec \"" $ do
            parser pDprec_ "%dprec" `parseSatisfies` (== PERCENT_DPREC)
            parser pDprec_ `shouldFailOn` "%dpreca"
            parser pDprec_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pDprec_ `isAtomicGiven` "%dpre"
    describe "pEmpty_" $ do
        it "should accept \"%empty \"" $ do
            parser pEmpty_ "%empty" `parseSatisfies` (== PERCENT_EMPTY)
            parser pEmpty_ `shouldFailOn` "%emptya"
            parser pEmpty_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pEmpty_ `isAtomicGiven` "%emp"
    describe "pErrorVerbose_" $ do
        it "should accept \"%error[-_]verbose \"" $ do
            parser pErrorVerbose_ "%error_verbose" `parseSatisfies` (== PERCENT_ERROR_VERBOSE)
            parser pErrorVerbose_ "%error-verbose" `parseSatisfies` (== PERCENT_ERROR_VERBOSE)
            parser pErrorVerbose_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pErrorVerbose_ `isAtomicGiven` "%error-"
    describe "pExpect_" $ do
        it "should accept \"%expect \"" $ do
            parser pExpect_ "%expect" `parseSatisfies` (== PERCENT_EXPECT)
            parser pExpect_ `shouldFailOn` "%expecta"
        it "should be atomic" $ do
            pExpect_ `isAtomicGiven` "%expec"
    describe "pExpectRr_" $ do
        it "should accept \"%expect[-_]rr \"" $ do
            parser pExpectRr_ "%expect-rr" `parseSatisfies` (== PERCENT_EXPECT_RR)
            parser pExpectRr_ "%expect_rr" `parseSatisfies` (== PERCENT_EXPECT_RR)
            parser pExpectRr_ `shouldFailOn` "%expect-r"
        it "should be atomic" $ do
            pExpectRr_ `isAtomicGiven` "%expect_r"
    describe "pFilePrefix_" $ do
        it "should accept \"%file-prefix(\\s*=)? \"" $ do
            parser pFilePrefix_ "%file-prefix\t= " `parseSatisfies` (== PERCENT_FILE_PREFIX)
            parser pFilePrefix_ `shouldFailOn` "%file-prefixa\t="
        it "should be atomic" $ do
            pFilePrefix_ `isAtomicGiven` "%file"
    describe "pInitialAction_" $ do
        it "should accept \"%initial-action \"" $ do
            parser pInitialAction_ "%initial-action" `parseSatisfies` (== PERCENT_INITIAL_ACTION)
            parser pInitialAction_ `shouldFailOn` "%initial-actiona"
        it "should be atomic" $ do
            pInitialAction_ `isAtomicGiven` "%init"
    describe "pGlrParser_" $ do
        it "should accept \"%glr-parser \"" $ do
            parser pGlrParser_ "%glr-parser" `parseSatisfies` (== PERCENT_GLR_PARSER)
            parser pGlrParser_ `shouldFailOn` "%glr-parsera"
        it "should be atomic" $ do
            pGlrParser_ `isAtomicGiven` "%glr-parseraf:"
    describe "pLanguage_" $ do
        it "should accept \"%language \"" $ do
            parser pLanguage_ "%language" `parseSatisfies` (== PERCENT_LANGUAGE)
            parser pLanguage_ `shouldFailOn` "%languagea"
        it "should be atomic" $ do
            pLanguage_ `isAtomicGiven` "%lang"
    describe "pLeft_" $ do
        it "should accept \"%left \"" $ do
            parser pLeft_ "%left" `parseSatisfies` (== PERCENT_LEFT)
            parser pLeft_ `shouldFailOn` "%lefta"
        it "should be atomic" $ do
            pLeft_ `isAtomicGiven` "%lefter"
    describe "pFlag_" $ do
        it "should accept \"%(debug|locations) \"" $ do
            parser pFlag_ "%debug" `parseSatisfies` (== PERCENT_FLAG "%debug")
            parser pFlag_ "%locations" `parseSatisfies` (== PERCENT_FLAG "%locations")
            parser pFlag_ `shouldFailOn` "%foo"
        it "should be atomic" $ do
            pFlag_ `isAtomicGiven` "%locat"
    describe "pMerge_" $ do
        it "should accept \"%merge \"" $do
            parser pMerge_ "%merge" `parseSatisfies` (== PERCENT_MERGE)
            parser pMerge_ `shouldFailOn` "%foo"
        it "should be atomic" $ do
            pMerge_ `isAtomicGiven` "%merg"
    describe "pNamePrefix_" $ do
        it "should accept \"%name[-_]prefix(\\s*=)? \"" $ do
            parser pNamePrefix_ "%name-prefix" `parseSatisfies` (== PERCENT_NAME_PREFIX)
            parser pNamePrefix_ "%name_prefix  \t=" `parseSatisfies` (== PERCENT_NAME_PREFIX)
            parser pNamePrefix_ `shouldFailOn` "%foo"
        it "should be atomic" $ do
            pNamePrefix_ `isAtomicGiven` "%name_-"
    describe "pNoDefaultPrec_" $ do
        it "should accept \"%no[-_]default[-_]prec \"" $ do
            parser pNoDefaultPrec_ "%no-default_prec" `parseSatisfies` (== PERCENT_NO_DEFAULT_PREC)
            parser pNoDefaultPrec_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pNoDefaultPrec_ `isAtomicGiven` "%no-default_pr"
    describe "pNoLines_" $ do
        it "should accept \"%no[-_]lines \"" $ do
            parser pNoLines_ "%no-lines" `parseSatisfies` (== PERCENT_NO_LINES)
            parser pNoLines_ "%no_lines" `parseSatisfies` (== PERCENT_NO_LINES)
            parser pNoLines_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pNoLines_ `isAtomicGiven` "%nolines"
    describe "pNonAssoc_" $ do
        it "should accept \"%(binary|nonassoc) \"" $ do
            parser pNonAssoc_ "%binary" `parseSatisfies` (== PERCENT_NONASSOC)
            parser pNonAssoc_ "%nonassoc" `parseSatisfies` (== PERCENT_NONASSOC)
            parser pNonAssoc_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pNonAssoc_ `isAtomicGiven` "%nonassocT"
    describe "pNonDeterministicParser_" $ do
        it "should accept \"%nondeterministic-parser \"" $ do
            parser pNonDeterministicParser_ "%nondeterministic-parser" `parseSatisfies` (== PERCENT_NONDETERMINISTIC_PARSER)
            parser pNonDeterministicParser_ `shouldFailOn` "foo"
        it "should be atomic" $ do
            pNonDeterministicParser_ `isAtomicGiven` "ndp"
    describe "pNterm_" $ do
        it "should accept \"%nterm \"" $ do
            parser pNterm_ "%nterm" `parseSatisfies` (== PERCENT_NTERM)
            parser pNterm_ `shouldFailOn` "%nterma"
        it "should be atomic" $ do
            pNterm_ `isAtomicGiven` "%nt?"
    describe "pOutput_" $ do
        it "should accept \"%output(\\s*=)? \"" $ do
            parser pOutput_ "%output" `parseSatisfies` (== PERCENT_OUTPUT)
            parser pOutput_ "%output =" `parseSatisfies` (== PERCENT_OUTPUT)
            parser pOutput_ `shouldFailOn` "%outputa ="
        it "should be atomic" $ do
            pOutput_ `isAtomicGiven` "%outpuT"
    describe "pFixedOutputFiles_" $ do
        it "should accept \"%fixed[-_]output[-_]files \"" $ do
            parser pFixedOutputFiles_ "%fixed-output_files" `parseSatisfies` (== PERCENT_FIXED_OUTPUT_FILES)
            parser pFixedOutputFiles_ `shouldFailOn` "%fixed-output-filesa"
        it "should be atomic" $ do
            pFixedOutputFiles_ `isAtomicGiven` "%fixed_output"
    describe "pParam_" $ do
        it "should accept \"%(param|parse-param|lex-param) \"" $ do
            parser pParam_ "%param" `parseSatisfies` (== PERCENT_PARAM "%param")
            parser pParam_ "%parse-param" `parseSatisfies` (== PERCENT_PARAM "%parse-param")
            parser pParam_ "%lex-param" `parseSatisfies` (== PERCENT_PARAM "%lex-param")
            parser pParam_ `shouldFailOn` "%parama"
        it "should be atomic" $ do
            pParam_ `isAtomicGiven` "%lex-parm"
    describe "pPrecedence_" $ do
        it "should accept \"%precedence \"" $ do
            parser pPrecedence_ "%precedence" `parseSatisfies` (== PERCENT_PRECEDENCE)
            parser pPrecedence_ `shouldFailOn` "%precedencea"
        it "should be atomic" $ do
            pPrecedence_ `isAtomicGiven` "%prec"
    describe "pPrec_" $ do
        it "should accept \"%prec \"" $ do
            parser pPrec_ "%prec" `parseSatisfies` (== PERCENT_PREC)
            parser pPrec_ `shouldFailOn` "%preca"
        it "should be atomic" $ do
            pPrec_ `isAtomicGiven` "%precedence"
    describe "pPrinter_" $ do
        it "should accept \"%printer \"" $ do
            parser pPrinter_ "%printer" `parseSatisfies` (== PERCENT_PRINTER)
            parser pPrinter_ `shouldFailOn` "%printera"
        it "should be atomic" $ do
            pPrinter_ `isAtomicGiven` "%print"
    describe "pPureParser_" $ do
        it "should accept \"%pure[-_]parser \"" $ do
            parser pPureParser_ "%pure-parser" `parseSatisfies` (== PERCENT_PURE_PARSER)
            parser pPureParser_ "%pure_parser" `parseSatisfies` (== PERCENT_PURE_PARSER)
            parser pPureParser_ `shouldFailOn` "%pure-parsera"
        it "should be atomic" $ do
            pPureParser_ `isAtomicGiven` "%pparser"
    describe "pRequire_" $ do
        it "should accept \"%require \"" $ do
            parser pRequire_ "%require" `parseSatisfies` (== PERCENT_REQUIRE)
            parser pRequire_ `shouldFailOn` "%requirea"
        it "should be atomic" $ do
            pRequire_ `isAtomicGiven` "%req"
    describe "pRight_" $ do
        it "should accept \"%right \"" $ do
            parser pRight_ "%right" `parseSatisfies` (== PERCENT_RIGHT)
            parser pRight_ `shouldFailOn` "%righta"
        it "should be atomic" $ do
            pRight_ `isAtomicGiven` "rt"
    describe "pSkeleton_" $ do
        it "should accept \"%skeleton \"" $ do
            parser pSkeleton_ "%skeleton" `parseSatisfies` (== PERCENT_SKELETON)
            parser pSkeleton_ `shouldFailOn` "%skeletona"
        it "should be atomic" $ do
            pSkeleton_ `isAtomicGiven` "%skelly"
    describe "pStart_" $ do
        it "should accept \"%start \"" $ do
            parser pStart_ "%start" `parseSatisfies` (== PERCENT_START)
            parser pStart_ `shouldFailOn` "%starta"
        it "should be atomic" $ do
            pStart_ `isAtomicGiven` "%tart"
    describe "pTokenTable_" $ do
        it "should accept \"%token[-_]table \"" $ do
            parser pTokenTable_ "%token-table" `parseSatisfies` (== PERCENT_TOKEN_TABLE)
            parser pTokenTable_ "%token_table" `parseSatisfies` (== PERCENT_TOKEN_TABLE)
            parser pTokenTable_ `shouldFailOn` "%token-tablea"
        it "should be atomic" $ do
            pTokenTable_ `isAtomicGiven` "%token-"
    describe "pToken_" $ do
        it "should accept \"%token \"" $ do
            parser pToken_ "%token" `parseSatisfies` (== PERCENT_TOKEN)
            parser pToken_ `shouldFailOn` "%tokena"
        it "should be atomic" $ do
            pToken_ `isAtomicGiven` "%to9ky"
    describe "pType_" $ do
        it "should accept \"%type \"" $ do
            parser pType_ "%type" `parseSatisfies` (== PERCENT_TYPE)
            parser pType_ `shouldFailOn` "%typea"
        it "should be atomic" $ do
            pType_ `isAtomicGiven` "%typer"
    describe "pUnion_" $ do
        it "should accept \"%union \"" $ do
            parser pUnion_ "%union" `parseSatisfies` (== PERCENT_UNION)
            parser pUnion_ `shouldFailOn` "%uniona"
        it "should be atomic" $ do
            pUnion_ `isAtomicGiven` "%unio!"
    describe "pVerbose_" $ do
        it "should accept \"%verbose \"" $ do
            parser pVerbose_ "%verbose" `parseSatisfies` (== PERCENT_VERBOSE)
            parser pVerbose_ `shouldFailOn` "%verbosea"
        it "should be atomic" $ do
            pVerbose_ `isAtomicGiven` "%vb"
    describe "pYacc_" $ do
        it "should accept \"%yacc \"" $ do
            parser pYacc_ "%yacc" `parseSatisfies` (== PERCENT_YACC)
            parser pYacc_ `shouldFailOn` "%yacca"
        it "should be atomic" $ do
            pYacc_ `isAtomicGiven` "%yc"
    describe "colon_" $ do
        it "should accept \":\"" $ do
            parser colon_ ":" `parseSatisfies` (== COLON)
            parser colon_ `shouldFailOn` "@"
        it "should be atomic" $ do
            colon_ `isAtomicGiven` "!"
    describe "equal_" $ do
        it "should accept \"=\"" $ do
            parser equal_ "=" `parseSatisfies` (== EQUAL)
            parser equal_ `shouldFailOn` "@"
        it "should be atomic" $ do
            equal_ `isAtomicGiven` "!"
    describe "pipe_" $ do
        it "should accept \"|\"" $ do
            parser pipe_ "|" `parseSatisfies` (== PIPE)
            parser pipe_ `shouldFailOn` "@"
        it "should be atomic" $ do
            pipe_ `isAtomicGiven` "!"
    describe "semicolon_" $ do
        it "should accept \";\"" $ do
            parser semicolon_ ";" `parseSatisfies` (== SEMICOLON)
            parser semicolon_ `shouldFailOn` "@"
        it "should be atomic" $ do
            semicolon_ `isAtomicGiven` "!"
    describe "identifier_" $ do
        it "should accept \"FooBar \"" $ do
            parser identifier_ "FooBar" `parseSatisfies` (== ID "FooBar")
        it "should accept \"_fooBar9 \"" $ do
            parser identifier_ "_FooBar9" `parseSatisfies` (== ID "_FooBar9")
        it "should not accept \"9fooBar \"" $ do
            parser identifier_ `shouldFailOn` "9fooBar"
        it "should not accept \"fooBar: \"" $ do
            parser identifier_ `shouldFailOn` "fooBar:"
        it "should not accept \"fooBar[ \"" $ do
            parser identifier_ `shouldFailOn` "fooBar[ "
        it "should be atomic" $ do
            identifier_ `isAtomicGiven` "foobar:"
    describe "idColon_" $ do
        it "should accept \"foobar: \"" $ do
            parser idColon_ "foobar:" `parseSatisfies` (== ID_COLON "foobar:")
        it "should be atomic" $ do
            idColon_ `isAtomicGiven` "foobar"
    describe "bracketedId_" $ do
        it "should accept \"[bar]\"" $ do
            parser bracketedId_ "[bar]" `parseSatisfies` (== BRACKETED_ID "[bar]")
        it "should accept \"_foo[_bar]\"" $ do
            parser bracketedId_ "_foo[bar]" `parseSatisfies` (== BRACKETED_ID "_foo[bar]")
        it "should not accept \"_foo[0_bar]\"" $ do
            parser bracketedId_ `shouldFailOn` "_foo[0_bar]"
        it "should be atomic" $ do
            bracketedId_ `isAtomicGiven` "foo[ba"
    describe "integer_" $ do
        it "should accept \"0[xX]A1F9\"" $ do
            parser integer_ "0xA159" `parseSatisfies` (== INT 41305)
            parser integer_ "0XA159" `parseSatisfies` (== INT 41305)
        it "should accept \"0314\"" $ do
            parser integer_ "0314" `parseSatisfies` (== INT 314)
        it "should not accept \"1AF\"" $ do
            parser integer_ `shouldFailOn` "1AF"
        it "should not accept \"19x\"" $ do
            parser integer_ `shouldFailOn` "19x"
        it "should be atomic" $ do
            integer_ `isAtomicGiven` "1Ax"
    describe "character_" $ do
        it "should accept \"'c'\"" $ do
            parser character_ "'c'" `parseSatisfies` (== CHAR 'c')
        it "should accept \"'0'\"" $ do
            parser character_ "'0'" `parseSatisfies` (== CHAR '0')
        it "should accept \"'\\t'\"" $ do
            parser character_ "'\\t'" `parseSatisfies` (== CHAR '\t')
        it "should accept \"'\\n'\"" $ do
            parser character_ "'\\n'" `parseSatisfies` (== CHAR '\n')
        it "should be atomic" $ do
            character_ `isAtomicGiven` "'a"
    describe "string_" $ do
        it "should accept \"\"foo \\t 0314 bar\"\"" $ do
            parser string_ "\"foo \\t 0134 bar\"" `parseSatisfies` (== STRING "foo \t 0134 bar")
        it "should be atomic" $ do
            string_ `isAtomicGiven` "\"foo\\t"
    describe "bracedCode_" $ do
        it "should accept \"{...}\"" $ do
            input <- bracedCodeFixture
            parser bracedCode_ input `parseSatisfies` isBracedCode
        it "should require a leading '{'" $ do
            parser bracedCode_ `shouldFailOn` "/*"
            parser bracedCode_ `shouldFailOn` "'"
            parser bracedCode_ `shouldFailOn` "\""
            parser bracedCode_ `shouldFailOn` "//\n"
            parser bracedCode_ `shouldFailOn` "f"
        it "should check for an extra trailing '}'" $ do
            parser bracedCode_ `shouldFailOn` "{{}}}"
        it "should not match braces in character literals" $ do
            parser bracedCode_ "{{ '{' }}" `parseSatisfies` isBracedCode
        it "should not match braces in string literals" $ do
            parser bracedCode_ "{{\"{\"}}" `parseSatisfies` isBracedCode
        it "should not match braces in line comments" $ do
            parser bracedCode_ "{{// {\n}}" `parseSatisfies` isBracedCode
        it "should not match braces in block comments" $ do
            parser bracedCode_ "{{/* {\n */}}" `parseSatisfies` isBracedCode
        it "should choke on unmatched braces" $ do
            parser bracedCode_ `shouldFailOn` "{ foo"
        it "should be atomic" $ do
            bracedCode_ `isAtomicGiven` "{{ //foo }\n int foo() {} bar"
    describe "tagAny_" $ do
        it "should accept \"<*>\"" $ do
            parser tagAny_ "<*>" `parseSatisfies` (== TAG_ANY)
        it "should be atomic" $ do
            tagAny_ `isAtomicGiven` "<*"
    describe "tagNone_" $ do
        it "should accept \"<>\"" $ do
            parser tagNone_ "<>" `parseSatisfies` (== TAG_NONE)
        it "should be atomic" $ do
            tagNone_ `isAtomicGiven` ">"
    describe "tag_" $ do
        it "should accept \"<foo<bar> -> baz>\"" $ do
            parser tag_ "<foo<bar> -> baz>" `parseSatisfies` (== TAG "<foo<bar> -> baz>")
        it "should choke on unmatched angle brackets" $ do
            parser tag_ `shouldFailOn` "<<"
        it "should be atomic" $ do
            tag_ `isAtomicGiven` "<foo -> bar>>"
    describe "epilogue_" $ do
        it "consume the rest of the input" $ do
            parser epilogue_ "foo" `parseSatisfies` (== EPILOGUE "foo")
    describe "token" $ do
        it "should prefer \"defines\" over \"define\"" $ do
            parser token "%defines" `parseSatisfies` (== PERCENT_DEFINES)
            parser token "%define" `parseSatisfies` (== PERCENT_DEFINE)
        it "should prefer \"expect-rr\" over \"expect\"" $ do
            parser token "%expect-rr" `parseSatisfies` (== PERCENT_EXPECT_RR)
            parser token "%expect" `parseSatisfies` (== PERCENT_EXPECT)
        it "should prefer \"precedence\" over \"prec\"" $ do
            parser token "%precedence" `parseSatisfies` (== PERCENT_PRECEDENCE)
            parser token "%prec" `parseSatisfies` (== PERCENT_PREC)
        it "should prefer \"token-table\" over \"token\"" $ do
            parser token "%token-table" `parseSatisfies` (== PERCENT_TOKEN_TABLE)
            parser token "%token" `parseSatisfies` (== PERCENT_TOKEN)
        it "should prefer \"identifier\" over \"identifier:\" over \"bracketed[identifier]\"" $ do
            parser token "_foo9Bar" `parseSatisfies` (== ID "_foo9Bar")
            parser token "_foo9Bar:" `parseSatisfies` (== ID_COLON "_foo9Bar:")
            parser token "_foo9[Bar]" `parseSatisfies` (== BRACKETED_ID "_foo9[Bar]")
