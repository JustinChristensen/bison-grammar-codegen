{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bison.Grammar.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token, runParser, runParser')
import Control.Applicative (Alternative, liftA2)
import Control.Monad (MonadPlus)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState)

newtype Parser a = Parser {
        runParser' :: StateT ParseState (Parsec Void Text) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
                MonadState ParseState, MonadParsec Void Text)

data ParseState = ParseState {
        section :: ScanSection
    } deriving (Show, Read, Eq)

data ScanSection
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

instance Semigroup a => Semigroup (Parser a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser a) where
    mempty = pure mempty
    mappend = (<>)

runParser :: ParseState -> Parser a -> Parsec Void Text a
runParser state = flip evalStateT state . runParser'
