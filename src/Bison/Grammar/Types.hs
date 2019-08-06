{-# LANGUAGE DeriveGeneric #-}
module Bison.Grammar.Types where

import GHC.Generics
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
    = SectionPrologue
    | SectionGrammar
    | SectionEpilogue
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
    deriving (Show, Read, Eq, Ord, Generic)

newtype StringT = StringT Text deriving (Show, Read, Eq, Ord, Generic)
newtype BracedCodeT = BracedCodeT Text deriving (Show, Read, Eq, Ord, Generic)
newtype BracedPredicateT = BracedPredicateT Text deriving (Show, Read, Eq, Ord, Generic)
newtype BracketedIdT = BracketedIdT Text deriving (Show, Read, Eq, Ord, Generic)
newtype CharT = CharT Char deriving (Show, Read, Eq, Ord, Generic)
newtype EpilogueT = EpilogueT Text deriving (Show, Read, Eq, Ord, Generic)
newtype IdT = IdT Text deriving (Show, Read, Eq, Ord, Generic)
newtype IdColonT = IdColonT Text deriving (Show, Read, Eq, Ord, Generic)
newtype IntT = IntT Int deriving (Show, Read, Eq, Ord, Generic)
newtype PrologueT = PrologueT Text deriving (Show, Read, Eq, Ord, Generic)
newtype PercentFlagT = PercentFlagT Text deriving (Show, Read, Eq, Ord, Generic)
newtype PercentParamT = PercentParamT Text deriving (Show, Read, Eq, Ord, Generic)
newtype TagT = TagT Text deriving (Show, Read, Eq, Ord, Generic)

data GrammarFile
    = GrammarFile [PrologueDecl] [GrammarRuleOrDecl] (Maybe Epilogue)
    deriving (Show, Read, Eq, Ord, Generic)

data PrologueDecl
    = GrammarDeclPD GrammarDecl
    | ProloguePD PrologueT
    | FlagPD PercentFlagT
    | DefinePD IdT (Maybe Value)
    | DefinesPD (Maybe StringT)
    | ErrorVerbosePD
    | ExpectPD IntT
    | ExpectRrPD IntT
    | FilePrefixPD StringT
    | GlrParserPD
    | InitialActionPD BracedCodeT
    | LanguagePD StringT
    | NamePrefixPD StringT
    | NoLinesPD
    | NonDeterministicParserPD
    | OutputPD StringT
    | ParamPD [BracedCodeT]
    | PureParserPD
    | RequirePD StringT
    | SkeletonPD StringT
    | TokenTablePD
    | VerbosePD
    | YaccPD
    deriving (Show, Read, Eq, Ord, Generic)

data GrammarDecl
    = SymbolDeclGD SymbolDecl
    | StartGD Symbol
    | DestructorGD BracedCodeT [GenericSymlistItem]
    | PrinterGD BracedCodeT [GenericSymlistItem]
    | DefaultPrecGD
    | NoDefaultPrecGD
    | CodeGD (Maybe IdT) BracedCodeT
    | UnionGD (Maybe IdT) BracedCodeT
    deriving (Show, Read, Eq, Ord, Generic)

data SymbolDecl
    = NTermSD (Maybe TagT) [TokenDecl] [TaggedDecls TokenDecl]
    | TokenSD (Maybe TagT) [TokenDecl] [TaggedDecls TokenDecl]
    | TypeSD (Maybe TagT) [Symbol] [TaggedDecls Symbol]
    | PrecedenceDeclSD PrecedenceDecl (Maybe TagT) [PrecTokenDecl] [TaggedDecls PrecTokenDecl]
    deriving (Show, Read, Eq, Ord, Generic)

data TaggedDecls d = TaggedDecls TagT [d]
    deriving (Show, Read, Eq, Ord, Generic)

data TokenDecl = TokenDecl Id (Maybe IntT) (Maybe StringT)
    deriving (Show, Read, Eq, Ord, Generic)

data PrecTokenDecl
    = IdP Id (Maybe IntT)
    | StrP StringT
    deriving (Show, Read, Eq, Ord, Generic)

data PrecedenceDecl
    = LeftPD
    | RightPD
    | NonAssocPD
    | PrecedencePD
    deriving (Show, Read, Eq, Ord, Generic)

data GenericSymlistItem
    = SymbolGS Symbol
    | TagGS Tag
    deriving (Show, Read, Eq, Ord, Generic)

data Tag
    = Tag TagT
    | TagAny
    | TagNone
    deriving (Show, Read, Eq, Ord, Generic)

data GrammarRuleOrDecl
    = RuleGD Rule
    | GrammarDeclGD GrammarDecl
    deriving (Show, Read, Eq, Ord, Generic)

data Rule = Rule IdColonT (Maybe BracketedIdT) [Rhses]
    deriving (Show, Read, Eq, Ord, Generic)

data Rhses = Rhses [Rhs]
    deriving (Show, Read, Eq, Ord, Generic)

data Rhs
    = SymR Symbol (Maybe BracketedIdT)
    | TagR (Maybe TagT) BracedCodeT (Maybe BracketedIdT)
    | PredR BracedPredicateT
    | EmptyR
    | PrecR Symbol
    | DprecR IntT
    | MergeR TagT
    | ExpectR IntT
    | ExpectRrR IntT
    deriving (Show, Read, Eq, Ord, Generic)

data Value
    = IdV IdT
    | StrV StringT
    | CodeV BracedCodeT
    deriving (Show, Read, Eq, Ord, Generic)

data Id
    = Id IdT
    | Char CharT
    deriving (Show, Read, Eq, Ord, Generic)

data Symbol
    = IdS Id
    | StrS StringT
    deriving (Show, Read, Eq, Ord, Generic)

data Epilogue
    = Epilogue EpilogueT
    deriving (Show, Read, Eq, Ord, Generic)

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
