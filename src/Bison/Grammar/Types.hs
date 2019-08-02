{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bison.Grammar.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Control.Applicative (Alternative, liftA2)
import Control.Monad (MonadPlus)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState)

type Parser = Parsec Void Text

newtype Scanner a = Scanner {
        runScanner' :: StateT ScanState Parser a
    } deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
                MonadState ScanState, MonadParsec Void Text)

data ScanState = ScanState {
        section :: ScanSection
    } deriving (Show, Read, Eq)

data ScanSection
    = Prologue
    | Grammar
    | Epilogue
    deriving (Show, Read, Eq, Ord, Enum)

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
    | PERCENT_FLAG Text
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
    | BRACED_CODE Text
    | BRACED_PREDICATE Text
    | BRACKETED_ID Text
    | CHAR Char
    | COLON
    | EPILOGUE Text
    | EQUAL
    | ID Text
    | ID_COLON Text
    | PERCENT_PERCENT
    | PIPE
    | PROLOGUE Text
    | SEMICOLON
    | TAG Text
    | TAG_ANY
    | TAG_NONE
    | INT Int
    | PERCENT_PARAM
    | PERCENT_UNION
    | PERCENT_EMPTY
    deriving (Show, Read, Eq, Ord)

instance Semigroup a => Semigroup (Scanner a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scanner a) where
    mempty = pure mempty
    mappend = (<>)

runScanner :: ScanState -> Scanner a -> Parser a
runScanner state = flip evalStateT state . runScanner'
