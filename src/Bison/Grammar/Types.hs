module Bison.Grammar.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token, runParser)
import Control.Applicative (liftA2)
import Control.Monad.State

type Parser = StateT ParseState (Parsec Void Text)

data ParseState = ParseState {
        section :: ParseSection
    } deriving (Show, Read, Eq)

data ParseSection
    = Prologue
    | Grammar
    | Epilogue
    deriving (Show, Read, Eq, Ord, Enum)

data Token
    = STRING Text
    | BRACED_CODE Text
    | BRACED_PREDICATE Text
    | BRACKETED_ID Text
    | CHAR Char
    | EPILOGUE Text
    | ID Text
    | ID_COLON Text
    | INT Int
    | PROLOGUE Text
    | PERCENT_FLAG Text
    | PERCENT_PARAM Text
    | TAG Text
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
    | PERCENT_FILE_PREFIX
    | PERCENT_FIXED_OUTPUT_FILES
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
    | COLON
    | EQUAL
    | PERCENT_PERCENT
    | PIPE
    | SEMICOLON
    | TAG_ANY
    | TAG_NONE
    | PERCENT_UNION
    | PERCENT_EMPTY
    deriving (Show, Read, Eq, Ord)

-- because StateT doesn't have a Semigroup instance and I don't want to wrap it
-- in a newtype just to add one, and AFAIK this doesn't already exist in base
infixr 6 #<>
(#<>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(#<>) = liftA2 (<>)

runParser :: ParseState -> Parser a -> (String -> Text -> Either (ParseErrorBundle Text Void) (a, ParseState))
runParser s p = parse (runStateT p s)

evalParser :: ParseState -> Parser a -> (String -> Text -> Either (ParseErrorBundle Text Void) a)
evalParser s p = parse (evalStateT p s)

execParser :: ParseState -> Parser a -> (String -> Text -> Either (ParseErrorBundle Text Void) ParseState)
execParser s p = parse (execStateT p s)
