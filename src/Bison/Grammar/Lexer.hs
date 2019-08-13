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

percentPercentS,
    predicateS, prologueS, pCodeS, pFlagS, pDefaultPrecS, pDefinesS, pDefineS, pDestructorS,
    pDprecS, pEmptyS, pErrorVerboseS, pExpectS, pExpectRrS, pFilePrefixS, pInitialActionS, pGlrParserS,
    pLanguageS, pLeftS, pMergeS, pNamePrefixS, pNoDefaultPrecS, pNoLinesS, pNonAssocS, pNonDeterministicParserS,
    pNtermS, pOutputS, pFixedOutputFilesS, pParamS, pPrecedenceS, pPrecS, pPrinterS, pPureParserS, pRequireS,
    pRightS, pSkeletonS, pStartS, pTokenTableS, pTokenS, pTypeS, pUnionS, pVerboseS, pYaccS, colonS, equalS,
    pipeS, semicolonS, identifierS, idColonS, bracketedIdS, integerS, characterS, stringS, bracedCodeS, tagAnyS,
    tagNoneS, tagS, epilogueS, token  :: Scanner Token

percentPercentS = lift percentPercent' *> do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

predicateS = lift predicate' <&> BRACED_PREDICATE
prologueS = lift prologue' <&> PROLOGUE
pCodeS = lift pCode' $> PERCENT_CODE
pFlagS = lift pFlag' <&> PERCENT_FLAG
pDefaultPrecS = lift pDefaultPrec' $> PERCENT_DEFAULT_PREC
pDefinesS = lift pDefines' $> PERCENT_DEFINES
pDefineS = lift pDefine' $> PERCENT_DEFINE
pDestructorS = lift pDestructor' $> PERCENT_DESTRUCTOR
pDprecS = lift pDprec' $> PERCENT_DPREC
pEmptyS = lift pEmpty' $> PERCENT_EMPTY
pErrorVerboseS = lift pErrorVerbose' $> PERCENT_ERROR_VERBOSE
pExpectS = lift pExpect' $> PERCENT_EXPECT
pExpectRrS = lift pExpectRr' $> PERCENT_EXPECT_RR
pFilePrefixS = lift pFilePrefix' $> PERCENT_FILE_PREFIX
pInitialActionS = lift pInitialAction' $> PERCENT_INITIAL_ACTION
pGlrParserS = lift pGlrParser' $> PERCENT_GLR_PARSER
pLanguageS = lift pLanguage' $> PERCENT_LANGUAGE
pLeftS = lift pLeft' $> PERCENT_LEFT
pMergeS = lift pMerge' $> PERCENT_MERGE
pNamePrefixS = lift pNamePrefix' $> PERCENT_NAME_PREFIX
pNoDefaultPrecS = lift pNoDefaultPrec' $> PERCENT_NO_DEFAULT_PREC
pNoLinesS = lift pNoLines' $> PERCENT_NO_LINES
pNonAssocS = lift (pBinary' <|> pNonAssoc') $> PERCENT_NONASSOC
pNonDeterministicParserS = lift pNonDeterministicParser' $> PERCENT_NONDETERMINISTIC_PARSER
pNtermS = lift pNterm' $> PERCENT_NTERM
pOutputS = lift pOutput' $> PERCENT_OUTPUT
pFixedOutputFilesS = lift pFixedOutputFiles' $> PERCENT_FIXED_OUTPUT_FILES
pParamS = lift pParam' <&> PERCENT_PARAM
pPrecedenceS = lift pPrecedence' $> PERCENT_PRECEDENCE
pPrecS = lift pPrec' $> PERCENT_PREC
pPrinterS = lift pPrinter' $> PERCENT_PRINTER
pPureParserS = lift pPureParser' $> PERCENT_PURE_PARSER
pRequireS = lift pRequire' $> PERCENT_REQUIRE
pRightS = lift pRight' $> PERCENT_RIGHT
pSkeletonS = lift pSkeleton' $> PERCENT_SKELETON
pStartS = lift pStart' $> PERCENT_START
pTokenTableS = lift pTokenTable' $> PERCENT_TOKEN_TABLE
pTokenS = lift (pTerm' <|> pToken') $> PERCENT_TOKEN
pTypeS = lift pType' $> PERCENT_TYPE
pUnionS = lift pUnion' $> PERCENT_UNION
pVerboseS = lift pVerbose' $> PERCENT_VERBOSE
pYaccS = lift pYacc' $> PERCENT_YACC
colonS = lift colon' $> COLON
equalS = lift equal' $> EQUAL
pipeS = lift pipe' $> PIPE
semicolonS = lift semicolon' $> SEMICOLON
identifierS = lift identifier' <&> ID
idColonS = lift idColon' <&> ID_COLON
bracketedIdS = lift bracketedId' <&> BRACKETED_ID
integerS = lift (hexadecimal' <|> decimal') <&> INT
characterS = lift character' <&> CHAR
stringS = lift string' <&> STRING
bracedCodeS = lift bracedCode' <&> BRACED_CODE
tagAnyS = lift tagAny' $> TAG_ANY
tagNoneS = lift tagNone' $> TAG_NONE
tagS = lift tag' <&> TAG
epilogueS = lift epilogue' <&> EPILOGUE

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


