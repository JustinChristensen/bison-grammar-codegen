{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
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

data GrammarFileF a
    = GrammarFile [PrologueDeclF a] a (GrammarF a) (Maybe a)
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data PrologueDeclF a
    = GrammarDeclPD (GrammarDeclF a)
    | ProloguePD a
    | FlagPD a
    | DefinePD a a (Maybe (ValueF a))
    | DefinesPD a
    | ErrorVerbosePD a
    | ExpectPD a a
    | ExpectRrPD a a
    | FilePrefixPD a a
    | GlrParserPD a
    | InitialActionPD a a
    | LanguagePD a a
    | NamePrefixPD a a
    | NoLinesPD a
    | NonDeterministicParserPD a
    | OutputPD a a
    | ParamPD a [a]
    | PureParserPD a
    | RequirePD a a
    | SkeletonPD a a
    | TokenTablePD a
    | VerbosePD a
    | YaccPD a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data GrammarDeclF a
    = SymbolDeclGD (SymbolDeclF a)
    | StartGD a (SymbolF a)
    | CodePropsTypeGD (CodePropsF a) a (GenericSymlistF a)
    | DefaultPrecGD a
    | NoDefaultPrecGD a
    | CodeGD a a
    | CodeIdGD a a a
    | UnionGD a a a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data CodePropsTypeF a
    = DestructorCP a
    | PrinterCP a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data SymbolDeclF a
    = NTermSD a [NTermDeclsF a]
    | TokenSD a [TokenDeclsF a]
    | TypeSD a [SymbolDeclsF a]
    | PrecedenceDeclSD (PrecedenceDeclF a) [TokenDeclsForPrecF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data PrecedenceDeclF a
    = LeftPD a
    | RightPD a
    | NonAssocPD a
    | PrecedencePD a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data GenericSymlistF a
    = SymbolGS (SymbolF a)
    | TagGS (TagF a)
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data TagF a
    = Tag a
    | TagAny a
    | TagNone a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

type NTermDeclsF = TokenDeclsF

data TokenDeclsF a
    = TokenDeclTD (Maybe a) [TokenDeclF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data TokenDeclF a
    = TokenDecl (IdF a) (Maybe a) (Maybe a)
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data TokenDeclsForPrecF a
    = TokenDeclsForPrec (Maybe a) [TokenDeclForPrecF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data TokenDeclForPrecF a
    = IdFP (IdF a) (Maybe a)
    | StrFP a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data SymbolDeclsF a
    = SymbolDecls (Maybe a) [SymbolDecl a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data SymbolDeclF a
    = SymbolDecl [SymbolF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data GrammarF a
    = RulesOrGrammarDecl [RulesOrGrammarDeclF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data RulesOrGrammarDeclF a
    = RulesRG [RulesF a]
    | GrammarDeclRG (GrammarDeclF a) a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data RulesF a
    = IdColonR a (Maybe a) a [RhsF a]
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data RhsF a
    = SymR (SymbolF a) (Maybe a)
    | TagR (Maybe (TagF a) a (Maybe a)
    | PredR a
    | EmptyR a
    | PrecR a (SymbolF a)
    | DprecR a a
    | MergeR a
    | ExpectR a
    | ExpectRrR a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data ValueF a
    = IdV a
    | StrV a
    | CodeV a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data IdF a
    = Id a
    | Char a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data SymbolF a
    = IdS (IdF a)
    | StrS a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

data EpilogueF a
    = Epilogue a a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

type GrammarFile = GrammarFileF Token
type PrologueDecl = PrologueDeclF Token
type GrammarDecl = GrammarDeclF Token
type CodePropsType = CodePropsTypeF Token
type SymbolDecl = SymbolDeclF Token
type PrecedenceDecl = PrecedenceDeclF Token
type GenericSymlist = GenericSymlistF Token
type Tag = TagF Token
type TokenDecls = TokenDeclsF Token
type TokenDecl = TokenDeclF Token
type TokenDeclsForPrec = TokenDeclsForPrecF Token
type TokenDeclForPrec = TokenDeclForPrecF Token
type SymbolDecls = SymbolDeclsF Token
type SymbolDecl = SymbolDeclF Token
type Grammar = GrammarF Token
type RulesOrGrammarDecl = RulesOrGrammarDeclF Token
type Rules = RulesF Token
type Rhs = RhsF Token
type Value = ValueF Token
type Id = IdF Token
type Symbol = SymbolF Token
type Epilogue = EpilogueF Token

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
