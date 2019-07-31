module Bison.Grammar.Types where

import Data.Text (Text)
import Data.Void (Void)
-- import Control.Monad.State
import Text.Megaparsec

type Scanner = Parsec Void Text
type Token = (TokenType, Text)

data ScannerState = ScannerState {
        section :: ScanSection
    }

data ScanSection
    = Prologue
    | Grammar
    | Epilogue
    deriving (Show, Read, Eq, Ord, Enum)

data ScanState
    = INITIAL
    | SC_YACC_COMMENT
    | SC_ESCAPED_STRING
    | SC_ESCAPED_CHARACTER
    | SC_AFTER_IDENTIFIER
    | SC_TAG
    | SC_PROLOGUE
    | SC_BRACED_CODE
    | SC_EPILOGUE
    | SC_PREDICATE
    | SC_COMMENT
    | SC_LINE_COMMENT
    | SC_STRING
    | SC_CHARACTER
    | SC_BRACKETED_ID
    | SC_RETURN_BRACKETED_ID
    deriving (Show, Read, Eq, Ord, Enum)

data TokenType
    = GRAM_EOF
    | STRING
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
    | BRACKETED_ID
    | CHAR
    | COLON
    | EPILOGUE
    | EQUAL
    | ID
    | ID_COLON
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
    deriving (Show, Read, Eq, Ord, Enum)

