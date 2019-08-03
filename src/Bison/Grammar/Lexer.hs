{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Functor
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intersperse)
import Control.Applicative hiding (many, some)
import Control.Monad.State (get, put, gets)
import Text.Megaparsec hiding (Token, token, tokens, runParser)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T (singleton, unpack, pack, map)
import Bison.Grammar.Types

test :: Char -> Parser Char
test = lookAhead . M.char

spaceOrEof :: Parser ()
spaceOrEof = (() <$ M.spaceChar) <|> eof

satisfyT :: (Char -> Bool) -> Parser Text
satisfyT f = T.singleton <$> satisfy f

anySingleT :: Parser Text
anySingleT = T.singleton <$> anySingle

charT :: Char -> Parser Text
charT c = T.singleton <$> M.char c

isLetter :: Char -> Bool
isLetter c = isAlpha c || c `elem` ['.', '_']

isLetterOrNum :: Char -> Bool
isLetterOrNum c = isLetter c || c == '-' || isDigit c

letter_ :: Parser Text
letter_ = satisfyT isLetter <?> "letter, '.', or '_'"

letterOrNum_ :: Parser Text
letterOrNum_ = satisfyT isLetterOrNum <?> "alphanumeric character, '.', or '_'"

id_ :: Parser Text
id_ = letter_ <>
    takeWhileP (Just "alphanumeric character, '.', or '_'") isLetterOrNum

spaces :: Parser Text
spaces = takeWhileP (Just "white space") isSpace

eqopt_ :: Parser Text
eqopt_ = option "" (spaces *> charT '=')

lineWithPrefix :: Text -> Parser Text
lineWithPrefix p = string p <> takeWhileP (Just "character") (/= '\n')

nested :: Text -> Text -> Parser Text -> Parser Text
nested start end content =
        start' <> (mconcat <$> manyTill content (lookAhead end')) <> end'
    where
        start' = string start
        end' = string end

kwsep :: [Char] -> [Text] -> Parser Text
kwsep seps strs = (T.map dashes <$> (try $ mconcat $
        intersperse (satisfyT (`elem` seps)) (string <$> strs)))
        <?> T.unpack (mconcat $ intersperse "-" strs)
    where
        dashes '_' = '-'
        dashes c = c

percentPercent :: Parser Token
percentPercent = token_ $ string "%%" >> do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

predicate' :: Parser Text
predicate' = try $ string "%?" <> spaces <> bracedCode'

predicate :: Parser Token
predicate =  token_ predicate' <&> BRACED_PREDICATE

prologue' :: Parser Text
prologue' = try $ charT '%' <> bracedCode'

prologue :: Parser Token
prologue = token_ prologue' <&> PROLOGUE

kwsep_ :: [Text] -> Parser Text
kwsep_ = kwsep "-_"

kw_ :: Text -> Parser Text
kw_ d = string d <* notFollowedBy letter_

pCode' :: Parser Text
pCode' = kw_ "%code"

pCode :: Parser Token
pCode = token_ pCode' $> PERCENT_CODE

pDebug' :: Parser Text
pDebug' = kw_ "%debug"

pLocations' :: Parser Text
pLocations' = kw_ "%locations"

pFlag :: Parser Token
pFlag = token_ (pDebug' <|> pLocations') <&> PERCENT_FLAG

pDefaultPrec' :: Parser Text
pDefaultPrec' = kwsep_ ["%default", "prec"]

pDefaultPrec :: Parser Token
pDefaultPrec = token_ pDefaultPrec' $> PERCENT_DEFAULT_PREC

pDefines' :: Parser Text
pDefines' = kw_ "%defines"

pDefines :: Parser Token
pDefines = token_ pDefines' $> PERCENT_DEFINES

pDefine' :: Parser Text
pDefine' = kw_ "%define"

pDefine :: Parser Token
pDefine = token_ pDefine' $> PERCENT_DEFINE

pDestructor' :: Parser Text
pDestructor' = kw_ "%destructor"

pDestructor :: Parser Token
pDestructor = token_ pDestructor' $> PERCENT_DESTRUCTOR

pDprec' :: Parser Text
pDprec' = kw_ "%dprec"

pDprec :: Parser Token
pDprec = token_ pDprec' $> PERCENT_DPREC

pEmpty' :: Parser Text
pEmpty' = kw_ "%empty"

pEmpty :: Parser Token
pEmpty = token_ pEmpty' $> PERCENT_EMPTY

pErrorVerbose' :: Parser Text
pErrorVerbose' = kwsep_ ["%error", "verbose"]

pErrorVerbose :: Parser Token
pErrorVerbose = token_ pErrorVerbose' $> PERCENT_ERROR_VERBOSE

pExpect' :: Parser Text
pExpect' = kw_ "%expect"

pExpect :: Parser Token
pExpect = token_ pExpect' $> PERCENT_EXPECT

pExpectRr' :: Parser Text
pExpectRr' = kwsep_ ["%expect", "rr"]

pExpectRr :: Parser Token
pExpectRr = token_ pExpectRr' $> PERCENT_EXPECT_RR

pFilePrefix' :: Parser Text
pFilePrefix' = try $ string "%file-prefix" <> eqopt_

pFilePrefix :: Parser Token
pFilePrefix = token_ pFilePrefix' $> PERCENT_FILE_PREFIX

pInitialAction' :: Parser Text
pInitialAction' = kw_ "%initial-action"

pInitialAction :: Parser Token
pInitialAction = token_ pInitialAction' $> PERCENT_INITIAL_ACTION

pGlrParser' :: Parser Text
pGlrParser' = kw_ "%glr-parser"

pGlrParser :: Parser Token
pGlrParser = token_ pGlrParser' $> PERCENT_GLR_PARSER

pLanguage' :: Parser Text
pLanguage' = kw_ "%language"

pLanguage :: Parser Token
pLanguage = token_ pLanguage' $> PERCENT_LANGUAGE

pLeft' :: Parser Text
pLeft' = kw_ "%left"

pLeft :: Parser Token
pLeft = token_ pLeft' $> PERCENT_LEFT

pMerge' :: Parser Text
pMerge' = kw_ "%merge"

pMerge :: Parser Token
pMerge = token_ pMerge' $> PERCENT_MERGE

pNamePrefix' :: Parser Text
pNamePrefix' = try (kwsep_ ["%name", "prefix"] <> eqopt_)

pNamePrefix :: Parser Token
pNamePrefix = pNamePrefix' $> PERCENT_NAME_PREFIX

pNoDefaultPrec' :: Parser Text
pNoDefaultPrec' = kwsep_ ["%no", "default", "prec"]

pNoDefaultPrec :: Parser Token
pNoDefaultPrec = token_ pNoDefaultPrec $> PERCENT_NO_DEFAULT_PREC

pNoLines' :: Parser Text
pNoLines' = kwsep_ ["%no", "lines"]

pNoLines :: Parser Token
pNoLines = token_ pNoLines' $> PERCENT_NO_LINES

pBinary' :: Parser Text
pBinary' = kw_ "%binary"

pNonAssoc' :: Parser Text
pNonAssoc' = kw_ "%nonassoc"

pNonAssoc :: Parser Token
pNonAssoc = token_ (pBinary' <|> pNonAssoc') $> PERCENT_NONASSOC

pNonDeterministicParser' :: Parser Text
pNonDeterministicParser' = kw_ "%nondeterministic-parser"

pNonDeterministicParser :: Parser Token
pNonDeterministicParser = token_ pNonDeterministicParser' $> PERCENT_NONDETERMINISTIC_PARSER

pNterm' :: Parser Text
pNterm' = kw_ "%nterm"

pNterm :: Parser Token
pNterm = token_ pNterm' $> PERCENT_NTERM

pOutput' :: Parser Text
pOutput' = try $ string "%output" <> eqopt_

pOutput :: Parser Token
pOutput = token_ pOutput' $> PERCENT_OUTPUT

pFixedOutputFiles' :: Parser Text
pFixedOutputFiles' = kwsep_ ["%fixed", "output", "files"]

pFixedOutputFiles :: Parser Token
pFixedOutputFiles = token_ pFixedOutputFiles' $> PERCENT_FIXED_OUTPUT_FILES

pParam' :: Parser Text
pParam' = kw_ "%param"

pLexParam' :: Parser Text
pLexParam' = kw_ "%lex-param"

pParseParam' :: Parser Text
pParseParam' = kw_ "%parse-param"

pParam :: Parser Token
pParam = token_ (pParam' <|> pParseParam' <|> pLexParam') <&> PERCENT_PARAM

pPrecedence' :: Parser Text
pPrecedence' = kw_ "%precedence"

pPrecedence :: Parser Token
pPrecedence = token_ pPrecedence' $> PERCENT_PRECEDENCE

pPrec' :: Parser Text
pPrec' = kw_ "%prec"

pPrec :: Parser Token
pPrec = token_ pPrec' $> PERCENT_PREC

pPrinter' :: Parser Text
pPrinter' = kw_ "%printer"

pPrinter :: Parser Token
pPrinter = token_ pPrinter' $> PERCENT_PRINTER

pPureParser' :: Parser Text
pPureParser' = kwsep_ ["%pure", "parser"]

pPureParser :: Parser Token
pPureParser = token_ pPureParser' $> PERCENT_PURE_PARSER

pRequire' :: Parser Text
pRequire' = kw_ "%require"

pRequire :: Parser Token
pRequire = token_ pRequire' $> PERCENT_REQUIRE

pRight' :: Parser Text
pRight' = kw_ "%right"

pRight :: Parser Token
pRight = token_ pRight' $> PERCENT_RIGHT

pSkeleton' :: Parser Text
pSkeleton' = kw_ "%skeleton"

pSkeleton :: Parser Token
pSkeleton = token_ pSkeleton' $> PERCENT_SKELETON

pStart' :: Parser Text
pStart' = kw_ "%start"

pStart :: Parser Token
pStart = token_ pStart' $> PERCENT_START

pTerm' :: Parser Text
pTerm' = kw_ "%term"

pTokenTable' :: Parser Text
pTokenTable' = kwsep_ ["%token", "table"]

pTokenTable :: Parser Token
pTokenTable = token_ pTokenTable' $> PERCENT_TOKEN_TABLE

pToken' :: Parser Text
pToken' = kw_ "%token"

pToken :: Parser Token
pToken = token_ (pTerm' <|> pToken') $> PERCENT_TOKEN

pType' :: Parser Text
pType' = kw_ "%type"

pType :: Parser Token
pType = token_ pType' $> PERCENT_TYPE

pUnion' :: Parser Text
pUnion' = kw_ "%union"

pUnion :: Parser Token
pUnion = token_ pUnion' $> PERCENT_UNION

pVerbose' :: Parser Text
pVerbose' = kw_ "%verbose"

pVerbose :: Parser Token
pVerbose = token_ pVerbose' $> PERCENT_VERBOSE

pYacc' :: Parser Text
pYacc' = kw_ "%yacc"

pYacc :: Parser Token
pYacc = token_ pYacc' $> PERCENT_YACC

colon :: Parser Token
colon = token_ (charT ':') $> COLON

equal :: Parser Token
equal = token_ (charT '=') $> EQUAL

pipe :: Parser Token
pipe = token_ (charT '|') $> PIPE

semicolon :: Parser Token
semicolon = token_ (charT ';') $> SEMICOLON

identifier' :: Parser Text
identifier' = id_ <* notFollowedBy letter_

identifier :: Parser Token
identifier = token_ identifier' <&> ID

idColon' :: Parser Text
idColon' = id_ <* test ':'

idColon :: Parser Token
idColon = token_ idColon' <&> ID_COLON

bracketedId' :: Parser Text
bracketedId' = try $ option "" id_ <> charT '[' <> id_ <> charT ']'

bracketedId :: Parser Token
bracketedId = bracketedId' <&> BRACKETED_ID

hexadecimal' :: Parser Int
hexadecimal' = try $ M.char '0' >> satisfy (`elem` ['x', 'X']) >>
    L.hexadecimal <* notFollowedBy letter_

decimal' :: Parser Int
decimal' = L.decimal <* notFollowedBy letter_

integer_ :: Parser Token
integer_ = token_ (hexadecimal' <|> decimal') <&> INT

character' :: Parser Char
character' = try $ M.char '\'' >> L.charLiteral <* M.char '\''

character_ :: Parser Token
character_ = character' <&> CHAR

string' :: Parser Text
string' = try $ M.char '"' >> (T.pack <$> manyTill L.charLiteral (M.char '"'))

string_ :: Parser Token
string_ = string' <&> STRING

bracedCode' :: Parser Text
bracedCode' = try $ test '{' >> (
            nested "{" "}" bracedCode'
        <|> (string' >>= wrap '"')
        <|> (T.singleton <$> character' >>= wrap '\'')
        <|> nested "/*" "*/" anySingleT
        <|> lineWithPrefix "//"
        <|> anySingleT)
    where
        wrap c str = let c' = T.singleton c
                    in pure $ c' <> str <> c'

bracedCode :: Parser Token
bracedCode = token_ bracedCode' <&> BRACED_CODE

tagAny' :: Parser Text
tagAny' = string "<*>"

tagAny :: Parser Token
tagAny = token_ tagAny' $> TAG_ANY

tagNone' :: Parser Text
tagNone' = string "<>"

tagNone :: Parser Token
tagNone = token_ tagNone' $> TAG_NONE

tag' :: Parser Text
tag' = test '<' >> (nested "<" ">" (try tag' <|> try (string "->") <|> satisfyT (`notElem` ['<', '>'])))

tag :: Parser Token
tag = token_ tag' <&> TAG

epilogue' :: Parser Text
epilogue' = takeRest <* eof

epilogue :: Parser Token
epilogue = token_ epilogue' <&> EPILOGUE

token :: Parser Token
token = do
    sect <- gets section
    case sect of
        Epilogue -> epilogue
        _ -> choice [
              percentPercent
            , prologue
            , predicate
            , pCode
            , pDefaultPrec
            , pDefines
            , pDefine
            , pDestructor
            , pDprec
            , pEmpty
            , pErrorVerbose
            , pExpect
            , pExpectRr
            , pFilePrefix
            , pInitialAction
            , pGlrParser
            , pLanguage
            , pLeft
            , pFlag
            , pMerge
            , pNamePrefix
            , pNoDefaultPrec
            , pNoLines
            , pNonAssoc
            , pNonDeterministicParser
            , pNterm
            , pOutput
            , pFixedOutputFiles
            , pParam
            , pPrecedence
            , pPrec
            , pPrinter
            , pPureParser
            , pRequire
            , pRight
            , pSkeleton
            , pStart
            , pTokenTable
            , pToken
            , pType
            , pUnion
            , pVerbose
            , pYacc
            , colon
            , equal
            , pipe
            , semicolon
            , identifier
            , idColon
            , bracketedId
            , integer_
            , character_
            , string_
            , bracedCode
            , tagAny
            , tagNone
            , tag
            ]

whitespace :: Parser ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

token_ :: Parser a -> Parser a
token_ = L.lexeme whitespace

tokens :: Parser [Token]
tokens = whitespace >> manyTill token eof

scan :: Text -> Maybe [Token]
scan = parseMaybe (runParser initialState tokens)
    where initialState = ParseState Prologue

