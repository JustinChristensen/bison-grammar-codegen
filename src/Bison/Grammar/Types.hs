{-# LANGUAGE DeriveGeneric #-}
module Bison.Grammar.Types where

import GHC.Generics
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Control.Monad.State
import Control.Monad.Reader

type Parser = Parsec Void Text
type Scanner = StateT ScanState Parser
type Codegen = ReaderT CodegenContext IO

data ScanState = ScanState {
        section :: ScanSection
    } deriving (Show, Read, Eq)

data ScanSection
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

newtype StringT = StringT Text
    deriving (Show, Read, Eq, Generic)
newtype BracedCodeT = BracedCodeT Text
    deriving (Show, Read, Eq, Generic)
newtype BracedPredicateT = BracedPredicateT Text
    deriving (Show, Read, Eq, Generic)
newtype BracketedIdT = BracketedIdT Text
    deriving (Show, Read, Eq, Generic)
newtype CharT = CharT Char
    deriving (Show, Read, Eq, Generic)
newtype EpilogueT = EpilogueT Text
    deriving (Show, Read, Eq, Generic)
newtype IdT = IdT Text
    deriving (Show, Read, Eq, Generic)
newtype IdColonT = IdColonT Text
    deriving (Show, Read, Eq, Generic)
newtype IntT = IntT Int
    deriving (Show, Read, Eq, Generic)
newtype PrologueT = PrologueT Text
    deriving (Show, Read, Eq, Generic)
newtype PercentFlagT = PercentFlagT Text
    deriving (Show, Read, Eq, Generic)
newtype PercentParamT = PercentParamT Text
    deriving (Show, Read, Eq, Generic)
newtype TagT = TagT Text
    deriving (Show, Read, Eq, Generic)

data GrammarFile
    = GrammarFile [PrologueDecl] [GrammarRuleOrDecl] (Maybe Epilogue)
    deriving (Show, Read, Eq, Generic)

data PrologueDecl
    = GrammarDeclPD GrammarDecl
    | ProloguePD PrologueT
    | FlagPD PercentFlagT
    | DefinePD IdT (Maybe ValueN)
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
    | ParamPD PercentParamT [BracedCodeT]
    | PureParserPD
    | RequirePD StringT
    | SkeletonPD StringT
    | TokenTablePD
    | VerbosePD
    | YaccPD
    deriving (Show, Read, Eq, Generic)

data GrammarDecl
    = SymbolDeclGD SymbolDecl
    | StartGD SymbolN
    | DestructorGD BracedCodeT [GenericSymlistItem]
    | PrinterGD BracedCodeT [GenericSymlistItem]
    | DefaultPrecGD
    | NoDefaultPrecGD
    | CodeGD (Maybe IdT) BracedCodeT
    | UnionGD (Maybe IdT) BracedCodeT
    deriving (Show, Read, Eq, Generic)

data SymbolDecl
    = NTermSD (Maybe TagT) [TokenDecl] [TaggedDecls TokenDecl]
    | TokenSD (Maybe TagT) [TokenDecl] [TaggedDecls TokenDecl]
    | TypeSD (Maybe TagT) [SymbolN] [TaggedDecls SymbolN]
    | PrecedenceDeclSD PrecedenceDecl (Maybe TagT) [PrecTokenDecl] [TaggedDecls PrecTokenDecl]
    deriving (Show, Read, Eq, Generic)

data TaggedDecls d = TaggedDecls TagT [d]
    deriving (Show, Read, Eq, Generic)

data TokenDecl = TokenDecl IdN (Maybe IntT) (Maybe StringT)
    deriving (Show, Read, Eq, Generic)

data PrecTokenDecl
    = IdP IdN (Maybe IntT)
    | StrP StringT
    deriving (Show, Read, Eq, Generic)

data PrecedenceDecl
    = LeftPD
    | RightPD
    | NonAssocPD
    | PrecedencePD
    deriving (Show, Read, Eq, Generic)

data GenericSymlistItem
    = SymbolGS SymbolN
    | TagGS Tag
    deriving (Show, Read, Eq, Generic)

data Tag
    = Tag TagT
    | TagAny
    | TagNone
    deriving (Show, Read, Eq, Generic)

data GrammarRuleOrDecl
    = RuleGD Rule
    | GrammarDeclGD GrammarDecl
    deriving (Show, Read, Eq, Generic)

data Rule = Rule IdColonT (Maybe BracketedIdT) [Rhses]
    deriving (Show, Read, Eq, Generic)

data Rhses = Rhses [Rhs]
    deriving (Show, Read, Eq, Generic)

data Rhs
    = SymR SymbolN (Maybe BracketedIdT)
    | TagR (Maybe TagT) BracedCodeT (Maybe BracketedIdT)
    | PredR BracedPredicateT
    | EmptyR
    | PrecR SymbolN
    | DprecR IntT
    | MergeR TagT
    | ExpectRrR IntT
    | ExpectR IntT
    deriving (Show, Read, Eq, Generic)

data ValueN
    = IdV IdT
    | StrV StringT
    | CodeV BracedCodeT
    deriving (Show, Read, Eq, Generic)

data IdN
    = Id IdT
    | Char CharT
    deriving (Show, Read, Eq, Generic)

data SymbolN
    = IdS IdN
    | StrS StringT
    deriving (Show, Read, Eq, Generic)

data Epilogue
    = Epilogue EpilogueT
    deriving (Show, Read, Eq, Generic)

data CodegenContext = CodegenContext {
        grammarAST :: GrammarFile
    } deriving (Show, Read, Eq, Generic)

data Production = Production {
        name :: Text
    ,   empty :: Bool
    ,   clauses :: [Clause]
    } deriving (Show, Read, Eq, Generic)

instance Semigroup Production where
    (Production n1 e1 cs1) <> (Production _ e2 cs2) =
        Production n1 (e1 || e2) (cs1 <> cs2)

instance Monoid Production where
    mempty = Production mempty False mempty
    mappend = (<>)

nameEq :: Production -> Production -> Bool
nameEq p1 p2 = name p1 == name p2

newtype Clause = Clause {
        symbols :: [Symbol]
    } deriving (Show, Read, Eq, Generic)

data Symbol
    = Terminal Text
    | NonTerminal Text
    deriving (Show, Read, Eq, Generic)

runScanner :: ScanState -> Scanner a -> (String -> Text -> Either (ParseErrorBundle Text Void) (a, ScanState))
runScanner s p = parse (runStateT p s)

evalScanner :: ScanState -> Scanner a -> (String -> Text -> Either (ParseErrorBundle Text Void) a)
evalScanner s p = parse (evalStateT p s)

execScanner :: ScanState -> Scanner a -> (String -> Text -> Either (ParseErrorBundle Text Void) ScanState)
execScanner s p = parse (execStateT p s)
