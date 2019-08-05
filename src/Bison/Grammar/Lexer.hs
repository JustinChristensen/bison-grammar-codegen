{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Functor
import Data.Void (Void)
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
id_ = letter_ #<>
    takeWhileP (Just "alphanumeric character, '.', or '_'") isLetterOrNum

spaces :: Parser Text
spaces = takeWhileP (Just "white space") isSpace

eqopt_ :: Parser Text
eqopt_ = option "" (spaces *> charT '=')

lineWithPrefix :: Text -> Parser Text
lineWithPrefix p = string p #<> takeWhileP (Just "character") (/= '\n')

nested :: Text -> Text -> Parser Text -> Parser Text
nested start end content =
        start' #<> (mconcat <$> manyTill content (lookAhead end')) #<> end'
    where
        start' = string start
        end' = string end

kwsep :: [Char] -> [Text] -> Parser Text
kwsep seps strs = T.map dashes <$> parser <?> T.unpack (mconcat $ intersperse "-" strs)
    where
        parser = try $ ((fmap mconcat . sequence) $
            intersperse (satisfyT (`elem` seps)) (string <$> strs)) <* notFollowedBy letter_
        dashes '_' = '-'
        dashes c = c

kwsep_ :: [Text] -> Parser Text
kwsep_ = kwsep "-_"

kw_ :: Text -> Parser Text
kw_ d = try $ string d <* notFollowedBy letter_

percentPercent_ :: Parser Token
percentPercent_ = makeToken $ string "%%" >> do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

predicate' :: Parser Text
predicate' = try $ string "%?" #<> spaces #<> bracedCode'

predicate_ :: Parser Token
predicate_ =  makeToken predicate' <&> BRACED_PREDICATE

prologue' :: Parser Text
prologue' = try $ charT '%' #<> bracedCode'

prologue_ :: Parser Token
prologue_ = makeToken prologue' <&> PROLOGUE

pCode' :: Parser Text
pCode' = kw_ "%code"

pCode_ :: Parser Token
pCode_ = makeToken pCode' $> PERCENT_CODE

pDebug' :: Parser Text
pDebug' = kw_ "%debug"

pLocations' :: Parser Text
pLocations' = kw_ "%locations"

pFlag_ :: Parser Token
pFlag_ = makeToken (pDebug' <|> pLocations') <&> PERCENT_FLAG

pDefaultPrec' :: Parser Text
pDefaultPrec' = kwsep_ ["%default", "prec"]

pDefaultPrec_ :: Parser Token
pDefaultPrec_ = makeToken pDefaultPrec' $> PERCENT_DEFAULT_PREC

pDefines' :: Parser Text
pDefines' = kw_ "%defines"

pDefines_ :: Parser Token
pDefines_ = makeToken pDefines' $> PERCENT_DEFINES

pDefine' :: Parser Text
pDefine' = kw_ "%define"

pDefine_ :: Parser Token
pDefine_ = makeToken pDefine' $> PERCENT_DEFINE

pDestructor' :: Parser Text
pDestructor' = kw_ "%destructor"

pDestructor_ :: Parser Token
pDestructor_ = makeToken pDestructor' $> PERCENT_DESTRUCTOR

pDprec' :: Parser Text
pDprec' = kw_ "%dprec"

pDprec_ :: Parser Token
pDprec_ = makeToken pDprec' $> PERCENT_DPREC

pEmpty' :: Parser Text
pEmpty' = kw_ "%empty"

pEmpty_ :: Parser Token
pEmpty_ = makeToken pEmpty' $> PERCENT_EMPTY

pErrorVerbose' :: Parser Text
pErrorVerbose' = kwsep_ ["%error", "verbose"]

pErrorVerbose_ :: Parser Token
pErrorVerbose_ = makeToken pErrorVerbose' $> PERCENT_ERROR_VERBOSE

pExpect' :: Parser Text
pExpect' = kw_ "%expect"

pExpect_ :: Parser Token
pExpect_ = makeToken pExpect' $> PERCENT_EXPECT

pExpectRr' :: Parser Text
pExpectRr' = kwsep_ ["%expect", "rr"]

pExpectRr_ :: Parser Token
pExpectRr_ = makeToken pExpectRr' $> PERCENT_EXPECT_RR

pFilePrefix' :: Parser Text
pFilePrefix' = try $ kw_ "%file-prefix" #<> eqopt_

pFilePrefix_ :: Parser Token
pFilePrefix_ = makeToken pFilePrefix' $> PERCENT_FILE_PREFIX

pInitialAction' :: Parser Text
pInitialAction' = kw_ "%initial-action"

pInitialAction_ :: Parser Token
pInitialAction_ = makeToken pInitialAction' $> PERCENT_INITIAL_ACTION

pGlrParser' :: Parser Text
pGlrParser' = kw_ "%glr-parser"

pGlrParser_ :: Parser Token
pGlrParser_ = makeToken pGlrParser' $> PERCENT_GLR_PARSER

pLanguage' :: Parser Text
pLanguage' = kw_ "%language"

pLanguage_ :: Parser Token
pLanguage_ = makeToken pLanguage' $> PERCENT_LANGUAGE

pLeft' :: Parser Text
pLeft' = kw_ "%left"

pLeft_ :: Parser Token
pLeft_ = makeToken pLeft' $> PERCENT_LEFT

pMerge' :: Parser Text
pMerge' = kw_ "%merge"

pMerge_ :: Parser Token
pMerge_ = makeToken pMerge' $> PERCENT_MERGE

pNamePrefix' :: Parser Text
pNamePrefix' = try (kwsep_ ["%name", "prefix"] #<> eqopt_)

pNamePrefix_ :: Parser Token
pNamePrefix_ = pNamePrefix' $> PERCENT_NAME_PREFIX

pNoDefaultPrec' :: Parser Text
pNoDefaultPrec' = kwsep_ ["%no", "default", "prec"]

pNoDefaultPrec_ :: Parser Token
pNoDefaultPrec_ = makeToken pNoDefaultPrec' $> PERCENT_NO_DEFAULT_PREC

pNoLines' :: Parser Text
pNoLines' = kwsep_ ["%no", "lines"]

pNoLines_ :: Parser Token
pNoLines_ = makeToken pNoLines' $> PERCENT_NO_LINES

pBinary' :: Parser Text
pBinary' = kw_ "%binary"

pNonAssoc' :: Parser Text
pNonAssoc' = kw_ "%nonassoc"

pNonAssoc_ :: Parser Token
pNonAssoc_ = makeToken (pBinary' <|> pNonAssoc') $> PERCENT_NONASSOC

pNonDeterministicParser' :: Parser Text
pNonDeterministicParser' = kw_ "%nondeterministic-parser"

pNonDeterministicParser_ :: Parser Token
pNonDeterministicParser_ = makeToken pNonDeterministicParser' $> PERCENT_NONDETERMINISTIC_PARSER

pNterm' :: Parser Text
pNterm' = kw_ "%nterm"

pNterm_ :: Parser Token
pNterm_ = makeToken pNterm' $> PERCENT_NTERM

pOutput' :: Parser Text
pOutput' = try $ kw_ "%output" #<> eqopt_

pOutput_ :: Parser Token
pOutput_ = makeToken pOutput' $> PERCENT_OUTPUT

pFixedOutputFiles' :: Parser Text
pFixedOutputFiles' = kwsep_ ["%fixed", "output", "files"]

pFixedOutputFiles_ :: Parser Token
pFixedOutputFiles_ = makeToken pFixedOutputFiles' $> PERCENT_FIXED_OUTPUT_FILES

pParam' :: Parser Text
pParam' = kw_ "%param"

pLexParam' :: Parser Text
pLexParam' = kw_ "%lex-param"

pParseParam' :: Parser Text
pParseParam' = kw_ "%parse-param"

pParam_ :: Parser Token
pParam_ = makeToken (pParam' <|> pParseParam' <|> pLexParam') <&> PERCENT_PARAM

pPrecedence' :: Parser Text
pPrecedence' = kw_ "%precedence"

pPrecedence_ :: Parser Token
pPrecedence_ = makeToken pPrecedence' $> PERCENT_PRECEDENCE

pPrec' :: Parser Text
pPrec' = kw_ "%prec"

pPrec_ :: Parser Token
pPrec_ = makeToken pPrec' $> PERCENT_PREC

pPrinter' :: Parser Text
pPrinter' = kw_ "%printer"

pPrinter_ :: Parser Token
pPrinter_ = makeToken pPrinter' $> PERCENT_PRINTER

pPureParser' :: Parser Text
pPureParser' = kwsep_ ["%pure", "parser"]

pPureParser_ :: Parser Token
pPureParser_ = makeToken pPureParser' $> PERCENT_PURE_PARSER

pRequire' :: Parser Text
pRequire' = kw_ "%require"

pRequire_ :: Parser Token
pRequire_ = makeToken pRequire' $> PERCENT_REQUIRE

pRight' :: Parser Text
pRight' = kw_ "%right"

pRight_ :: Parser Token
pRight_ = makeToken pRight' $> PERCENT_RIGHT

pSkeleton' :: Parser Text
pSkeleton' = kw_ "%skeleton"

pSkeleton_ :: Parser Token
pSkeleton_ = makeToken pSkeleton' $> PERCENT_SKELETON

pStart' :: Parser Text
pStart' = kw_ "%start"

pStart_ :: Parser Token
pStart_ = makeToken pStart' $> PERCENT_START

pTerm' :: Parser Text
pTerm' = kw_ "%term"

pTokenTable' :: Parser Text
pTokenTable' = kwsep_ ["%token", "table"]

pTokenTable_ :: Parser Token
pTokenTable_ = makeToken pTokenTable' $> PERCENT_TOKEN_TABLE

pToken' :: Parser Text
pToken' = kw_ "%token"

pToken_ :: Parser Token
pToken_ = makeToken (pTerm' <|> pToken') $> PERCENT_TOKEN

pType' :: Parser Text
pType' = kw_ "%type"

pType_ :: Parser Token
pType_ = makeToken pType' $> PERCENT_TYPE

pUnion' :: Parser Text
pUnion' = kw_ "%union"

pUnion_ :: Parser Token
pUnion_ = makeToken pUnion' $> PERCENT_UNION

pVerbose' :: Parser Text
pVerbose' = kw_ "%verbose"

pVerbose_ :: Parser Token
pVerbose_ = makeToken pVerbose' $> PERCENT_VERBOSE

pYacc' :: Parser Text
pYacc' = kw_ "%yacc"

pYacc_ :: Parser Token
pYacc_ = makeToken pYacc' $> PERCENT_YACC

colon_ :: Parser Token
colon_ = makeToken (charT ':') $> COLON

equal_ :: Parser Token
equal_ = makeToken (charT '=') $> EQUAL

pipe_ :: Parser Token
pipe_ = makeToken (charT '|') $> PIPE

semicolon_ :: Parser Token
semicolon_ = makeToken (charT ';') $> SEMICOLON

identifier' :: Parser Text
identifier' = try $ id_ <* notFollowedBy (letter_ <|> charT ':' <|> charT '[')

identifier_ :: Parser Token
identifier_ = makeToken identifier' <&> ID

idColon' :: Parser Text
idColon' = try $ id_ <* test ':'

idColon_ :: Parser Token
idColon_ = ((<> ":") <$> makeToken idColon') <&> ID_COLON

bracketedId' :: Parser Text
bracketedId' = try $ option "" id_ #<> charT '[' #<> id_ #<> charT ']'

bracketedId_ :: Parser Token
bracketedId_ = makeToken bracketedId' <&> BRACKETED_ID

hexadecimal' :: Parser Int
hexadecimal' = try $ M.char '0' >> satisfy (`elem` ['x', 'X']) >>
    L.hexadecimal <* notFollowedBy letter_

decimal' :: Parser Int
decimal' = try $ L.decimal <* notFollowedBy letter_

integer_ :: Parser Token
integer_ = makeToken (hexadecimal' <|> decimal') <&> INT

character' :: Parser Char
character' = try $ M.char '\'' >> L.charLiteral <* M.char '\''

character_ :: Parser Token
character_ = makeToken character' <&> CHAR

string' :: Parser Text
string' = try $ M.char '"' >> (T.pack <$> manyTill L.charLiteral (M.char '"'))

string_ :: Parser Token
string_ = makeToken string' <&> STRING

bracedCode' :: Parser Text
bracedCode' = try $ nested "{" "}" p <* notFollowedBy (M.char '}')
    where
        wrap c str = let c' = T.singleton c
                    in pure $ c' <> str <> c'
        p = nested "{" "}" p
            <|> (string' >>= wrap '"')
            <|> (T.singleton <$> character' >>= wrap '\'')
            <|> nested "/*" "*/" anySingleT
            <|> lineWithPrefix "//"
            <|> anySingleT

bracedCode_ :: Parser Token
bracedCode_ = makeToken bracedCode' <&> BRACED_CODE

tagAny' :: Parser Text
tagAny' = string "<*>"

tagAny_ :: Parser Token
tagAny_ = makeToken tagAny' $> TAG_ANY

tagNone' :: Parser Text
tagNone' = string "<>"

tagNone_ :: Parser Token
tagNone_ = makeToken tagNone' $> TAG_NONE

tag' :: Parser Text
tag' = try $ nested "<" ">" p <* notFollowedBy (M.char '>')
    where
        p = nested "<" ">" p
            <|> string "->"
            <|> satisfyT (`notElem` ['<', '>'])

tag_ :: Parser Token
tag_ = makeToken tag' <&> TAG

epilogue' :: Parser Text
epilogue' = takeRest <* eof

epilogue_ :: Parser Token
epilogue_ = makeToken epilogue' <&> EPILOGUE

token :: Parser Token
token = do
    sect <- gets section
    case sect of
        SectionEpilogue -> epilogue_
        _ -> choice [
              percentPercent_
            , prologue_
            , predicate_
            , pCode_
            , pDefaultPrec_
            , pDefines_
            , pDefine_
            , pDestructor_
            , pDprec_
            , pEmpty_
            , pErrorVerbose_
            , pExpectRr_
            , pExpect_
            , pFilePrefix_
            , pInitialAction_
            , pGlrParser_
            , pLanguage_
            , pLeft_
            , pFlag_
            , pMerge_
            , pNamePrefix_
            , pNoDefaultPrec_
            , pNoLines_
            , pNonAssoc_
            , pNonDeterministicParser_
            , pNterm_
            , pOutput_
            , pFixedOutputFiles_
            , pParam_
            , pPrecedence_
            , pPrec_
            , pPrinter_
            , pPureParser_
            , pRequire_
            , pRight_
            , pSkeleton_
            , pStart_
            , pTokenTable_
            , pToken_
            , pType_
            , pUnion_
            , pVerbose_
            , pYacc_
            , colon_
            , equal_
            , pipe_
            , semicolon_
            , identifier_
            , idColon_
            , bracketedId_
            , integer_
            , character_
            , string_
            , bracedCode_
            , tagAny_
            , tagNone_
            , tag_
            ]

whitespace :: Parser ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

makeToken :: Parser a -> Parser a
makeToken = L.lexeme whitespace

tokens :: Parser [Token]
tokens = whitespace >> manyTill token eof

scan :: Text -> Either (ParseErrorBundle Text Void) [Token]
scan = evalParser initialState tokens ""
    where initialState = ParseState SectionPrologue

