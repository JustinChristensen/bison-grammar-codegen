module Bison.Grammar.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)

type Scanner = Parsec Void Text
-- type Token = (TokenType, Text)

-- data ScanSection
--     = Prologue
--     | Grammar
--     | Epilogue
--     deriving (Show, Read, Eq, Ord, Enum)

data Token
    = STRING Text
    | PERCENT_TOKEN
    | PERCENT_NTERM
    | PERCENT_TYPE
    | PERCENT_DESTRUCTOR
    | PERCENT_PRINTER
    | PERCENT_LEFT
    | PERCENT_RIGHT
    | PERCENT_NONASSOC
    | PERCENT_PRECEDENCE
    | PERCENT_PREC
    | PERCENT_DPREC
    | PERCENT_MERGE
    | PERCENT_CODE
    | PERCENT_DEFAULT_PREC
    | PERCENT_DEFINE
    | PERCENT_DEFINES
    | PERCENT_ERROR_VERBOSE
    | PERCENT_EXPECT
    | PERCENT_EXPECT_RR
    | PERCENT_FLAG
    | PERCENT_FILE_PREFIX
    | PERCENT_GLR_PARSER
    | PERCENT_INITIAL_ACTION
    | PERCENT_LANGUAGE
    | PERCENT_NAME_PREFIX
    | PERCENT_NO_DEFAULT_PREC
    | PERCENT_NO_LINES
    | PERCENT_NONDETERMINISTIC_PARSER
    | PERCENT_OUTPUT
    | PERCENT_PURE_PARSER
    | PERCENT_REQUIRE
    | PERCENT_SKELETON
    | PERCENT_START
    | PERCENT_TOKEN_TABLE
    | PERCENT_VERBOSE
    | PERCENT_YACC
    | BRACED_CODE
    | BRACED_PREDICATE
    | BRACKETED_ID Text
    | CHAR
    | COLON
    | EPILOGUE
    | EQUAL
    | ID Text
    | ID_COLON Text
    | PERCENT_PERCENT
    | PIPE
    | PROLOGUE
    | SEMICOLON
    | TAG
    | TAG_ANY
    | TAG_NONE
    | INT
    | PERCENT_PARAM
    | PERCENT_UNION
    | PERCENT_EMPTY
    deriving (Show, Read, Eq, Ord)

