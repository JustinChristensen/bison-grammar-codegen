{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Functor
import Data.Void (Void)
import Control.Applicative hiding (many, some)
import Control.Monad.State (get, put, gets, lift)
import Text.Megaparsec hiding (Token, token, tokens)
import Data.Text (Text)
import Bison.Grammar.Types
import Bison.Grammar.Parser

percentPercentS :: Scanner Token
percentPercentS = lift percentPercent' *> do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

predicateS :: Scanner Token
predicateS = lift predicate' <&> BRACED_PREDICATE

prologueS :: Scanner Token
prologueS = lift prologue' <&> PROLOGUE

pCodeS :: Scanner Token
pCodeS = lift pCode' $> PERCENT_CODE

pFlagS :: Scanner Token
pFlagS = lift pFlag' <&> PERCENT_FLAG

pDefaultPrecS :: Scanner Token
pDefaultPrecS = lift pDefaultPrec' $> PERCENT_DEFAULT_PREC

pDefinesS :: Scanner Token
pDefinesS = lift pDefines' $> PERCENT_DEFINES

pDefineS :: Scanner Token
pDefineS = lift pDefine' $> PERCENT_DEFINE

pDestructorS :: Scanner Token
pDestructorS = lift pDestructor' $> PERCENT_DESTRUCTOR

pDprecS :: Scanner Token
pDprecS = lift pDprec' $> PERCENT_DPREC

pEmptyS :: Scanner Token
pEmptyS = lift pEmpty' $> PERCENT_EMPTY

pErrorVerboseS :: Scanner Token
pErrorVerboseS = lift pErrorVerbose' $> PERCENT_ERROR_VERBOSE

pExpectS :: Scanner Token
pExpectS = lift pExpect' $> PERCENT_EXPECT

pExpectRrS :: Scanner Token
pExpectRrS = lift pExpectRr' $> PERCENT_EXPECT_RR

pFilePrefixS :: Scanner Token
pFilePrefixS = lift pFilePrefix' $> PERCENT_FILE_PREFIX

pInitialActionS :: Scanner Token
pInitialActionS = lift pInitialAction' $> PERCENT_INITIAL_ACTION

pGlrParserS :: Scanner Token
pGlrParserS = lift pGlrParser' $> PERCENT_GLR_PARSER

pLanguageS :: Scanner Token
pLanguageS = lift pLanguage' $> PERCENT_LANGUAGE

pLeftS :: Scanner Token
pLeftS = lift pLeft' $> PERCENT_LEFT

pMergeS :: Scanner Token
pMergeS = lift pMerge' $> PERCENT_MERGE

pNamePrefixS :: Scanner Token
pNamePrefixS = lift pNamePrefix' $> PERCENT_NAME_PREFIX

pNoDefaultPrecS :: Scanner Token
pNoDefaultPrecS = lift pNoDefaultPrec' $> PERCENT_NO_DEFAULT_PREC

pNoLinesS :: Scanner Token
pNoLinesS = lift pNoLines' $> PERCENT_NO_LINES

pNonAssocS :: Scanner Token
pNonAssocS = lift (pBinary' <|> pNonAssoc') $> PERCENT_NONASSOC

pNonDeterministicParserS :: Scanner Token
pNonDeterministicParserS = lift pNonDeterministicParser' $> PERCENT_NONDETERMINISTIC_PARSER

pNtermS :: Scanner Token
pNtermS = lift pNterm' $> PERCENT_NTERM

pOutputS :: Scanner Token
pOutputS = lift pOutput' $> PERCENT_OUTPUT

pFixedOutputFilesS :: Scanner Token
pFixedOutputFilesS = lift pFixedOutputFiles' $> PERCENT_FIXED_OUTPUT_FILES

pParamS :: Scanner Token
pParamS = lift pParam' <&> PERCENT_PARAM

pPrecedenceS :: Scanner Token
pPrecedenceS = lift pPrecedence' $> PERCENT_PRECEDENCE

pPrecS :: Scanner Token
pPrecS = lift pPrec' $> PERCENT_PREC

pPrinterS :: Scanner Token
pPrinterS = lift pPrinter' $> PERCENT_PRINTER

pPureParserS :: Scanner Token
pPureParserS = lift pPureParser' $> PERCENT_PURE_PARSER

pRequireS :: Scanner Token
pRequireS = lift pRequire' $> PERCENT_REQUIRE

pRightS :: Scanner Token
pRightS = lift pRight' $> PERCENT_RIGHT

pSkeletonS :: Scanner Token
pSkeletonS = lift pSkeleton' $> PERCENT_SKELETON

pStartS :: Scanner Token
pStartS = lift pStart' $> PERCENT_START

pTokenTableS :: Scanner Token
pTokenTableS = lift pTokenTable' $> PERCENT_TOKEN_TABLE

pTokenS :: Scanner Token
pTokenS = lift (pTerm' <|> pToken') $> PERCENT_TOKEN

pTypeS :: Scanner Token
pTypeS = lift pType' $> PERCENT_TYPE

pUnionS :: Scanner Token
pUnionS = lift pUnion' $> PERCENT_UNION

pVerboseS :: Scanner Token
pVerboseS = lift pVerbose' $> PERCENT_VERBOSE

pYaccS :: Scanner Token
pYaccS = lift pYacc' $> PERCENT_YACC

colonS :: Scanner Token
colonS = lift colon' $> COLON

equalS :: Scanner Token
equalS = lift equal' $> EQUAL

pipeS :: Scanner Token
pipeS = lift pipe' $> PIPE

semicolonS :: Scanner Token
semicolonS = lift semicolon' $> SEMICOLON

identifierS :: Scanner Token
identifierS = lift identifier' <&> ID

idColonS :: Scanner Token
idColonS = lift idColon' <&> ID_COLON

bracketedIdS :: Scanner Token
bracketedIdS = lift bracketedId' <&> BRACKETED_ID

integerS :: Scanner Token
integerS = lift (hexadecimal' <|> decimal') <&> INT

characterS :: Scanner Token
characterS = lift character' <&> CHAR

stringS :: Scanner Token
stringS = lift string' <&> STRING

bracedCodeS :: Scanner Token
bracedCodeS = lift bracedCode' <&> BRACED_CODE

tagAnyS :: Scanner Token
tagAnyS = lift tagAny' $> TAG_ANY

tagNoneS :: Scanner Token
tagNoneS = lift tagNone' $> TAG_NONE

tagS :: Scanner Token
tagS = lift tag' <&> TAG

epilogueS :: Scanner Token
epilogueS = lift epilogue' <&> EPILOGUE

token :: Scanner Token
token = do
    sect <- gets section
    case sect of
        SectionEpilogue -> epilogueS
        _ -> choice [
              percentPercentS
            , prologueS
            , predicateS
            , pCodeS
            , pDefaultPrecS
            , pDefinesS
            , pDefineS
            , pDestructorS
            , pDprecS
            , pEmptyS
            , pErrorVerboseS
            , pExpectRrS
            , pExpectS
            , pFilePrefixS
            , pInitialActionS
            , pGlrParserS
            , pLanguageS
            , pLeftS
            , pFlagS
            , pMergeS
            , pNamePrefixS
            , pNoDefaultPrecS
            , pNoLinesS
            , pNonAssocS
            , pNonDeterministicParserS
            , pNtermS
            , pOutputS
            , pFixedOutputFilesS
            , pParamS
            , pPrecedenceS
            , pPrecS
            , pPrinterS
            , pPureParserS
            , pRequireS
            , pRightS
            , pSkeletonS
            , pStartS
            , pTokenTableS
            , pTokenS
            , pTypeS
            , pUnionS
            , pVerboseS
            , pYaccS
            , colonS
            , equalS
            , pipeS
            , semicolonS
            , identifierS
            , idColonS
            , bracketedIdS
            , integerS
            , characterS
            , stringS
            , bracedCodeS
            , tagAnyS
            , tagNoneS
            , tagS
            ]

tokens :: Scanner [Token]
tokens = lift whitespace >> manyTill token eof

scan :: Text -> Either (ParseErrorBundle Text Void) [Token]
scan = evalScanner initialState tokens ""
    where initialState = ScanState SectionPrologue

