{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Char (isAlpha, isDigit, isHexDigit, isSpace)
import Data.List (intersperse)
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (Token, token, tokens)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T (singleton, map)
import Bison.Grammar.Types

spaceOrEof :: Scanner ()
spaceOrEof = (() <$ M.spaceChar) <|> eof

satisfyT :: (Char -> Bool) -> Scanner Text
satisfyT f = T.singleton <$> satisfy f

charT :: Char -> Scanner Text
charT c = T.singleton <$> M.char c

isLetter :: Char -> Bool
isLetter c = isAlpha c || c `elem` ['.', '_']

isLetterOrNum :: Char -> Bool
isLetterOrNum c = isLetter c || c == '-' || isDigit c

letter_ :: Scanner Text
letter_ = satisfyT isLetter <?> "letter, '.', or '_'"

id_ :: Scanner Text
id_ = letter_ <>
    takeWhileP (Just "alphanumeric character, '.', or '_'") isLetterOrNum

int_ :: Scanner Text
int_ = takeWhile1P (Just "digit") isDigit

xint_ :: Scanner Text
xint_ = try ((charT '0')
        <> (satisfyT (`elem` ['x', 'X']) <?> "'x', or 'x'")
        <> takeWhile1P (Just "hex digit") isHexDigit)

splice_ :: Scanner Text
splice_ = undefined

sp_ :: Scanner Text
sp_ = takeWhileP (Just "whitespace") (isSpace)

eqopt_ :: Scanner Text
eqopt_ = option "" (sp_ <> charT '=')

-- infixr 7 &#
-- (&#) :: Scanner Text -> TokenType -> Scanner Token
-- s &# t = (,) t <$> s

sep_ :: [Char] -> [Text] -> Scanner Text
sep_ seps strs = T.map dashes <$> (try $ mconcat $
        intersperse (satisfyT (`elem` seps)) (string <$> strs))
    where
        dashes '_' = '-'
        dashes c = c

declarations :: Scanner Token
declarations = let seps = "-_" in choice
    [
      PERCENT_NONASSOC                    <$ string "%binary"
    , PERCENT_CODE                        <$ string "%code"
    , PERCENT_FLAG                        <$ string "%debug"
    , PERCENT_DEFAULT_PREC                <$ sep_ seps ["%default", "prec"]
    , PERCENT_DEFINES                     <$ string "%defines"
    , PERCENT_DEFINE                      <$ string "%define"
    , PERCENT_DESTRUCTOR                  <$ string "%destructor"
    , PERCENT_DPREC                       <$ string "%dprec"
    , PERCENT_EMPTY                       <$ string "%empty"
    , PERCENT_ERROR_VERBOSE               <$ sep_ seps ["%error", "verbose"] -- deprecated
    , PERCENT_EXPECT                      <$ string "%expect"
    , PERCENT_EXPECT_RR                   <$ sep_ seps ["%expect", "rr"] -- deprecated
    , PERCENT_FILE_PREFIX                 <$ try (string "%file-prefix" <> eqopt_) -- deprecated
    , PERCENT_INITIAL_ACTION              <$ string "%initial-action"
    , PERCENT_GLR_PARSER                  <$ string "%glr-parser"
    , PERCENT_LANGUAGE                    <$ string "%language"
    , PERCENT_LEFT                        <$ string "%left"
    , PERCENT_PARAM                       <$ string "%lex-param"
    , PERCENT_FLAG                        <$ string "%locations"
    , PERCENT_MERGE                       <$ string "%merge"
    , PERCENT_NAME_PREFIX                 <$ try (sep_ seps ["%name", "prefix"] <> eqopt_ <> sp_)  -- deprecated
    , PERCENT_NO_DEFAULT_PREC             <$ sep_ seps ["%no", "default", "prec"] -- deprecated
    , PERCENT_NO_LINES                    <$ sep_ seps ["%no", "lines"] -- deprecated
    , PERCENT_NONASSOC                    <$ string "%nonassoc"
    , PERCENT_NONDETERMINISTIC_PARSER     <$ string "%nondeterministic-parser"
    , PERCENT_NTERM                       <$ string "%nterm"
    , PERCENT_OUTPUT                      <$ try (string "%output" <> eqopt_) -- deprecated
    -- , sep_ seps ["%fixed", "output", "files"]       DEPRECATED ("%output \"y.tab.c\"");
    , PERCENT_PARAM                       <$ string "%param"
    , PERCENT_PARAM                       <$ string "%parse-param"
    , PERCENT_PREC                        <$ string "%prec"
    , PERCENT_PRECEDENCE                  <$ string "%precedence"
    , PERCENT_PRINTER                     <$ string "%printer"
    , PERCENT_PURE_PARSER                 <$ sep_ seps ["%pure", "parser"] -- deprecated
    , PERCENT_REQUIRE                     <$ string "%require"
    , PERCENT_RIGHT                       <$ string "%right"
    , PERCENT_SKELETON                    <$ string "%skeleton"
    , PERCENT_START                       <$ string "%start"
    , PERCENT_TOKEN                       <$ string "%term"
    , PERCENT_TOKEN                       <$ string "%token"
    , PERCENT_TOKEN_TABLE                 <$ sep_ seps ["%token", "table"] -- deprecated
    , PERCENT_TYPE                        <$ string "%type"
    , PERCENT_UNION                       <$ string "%union"
    , PERCENT_VERBOSE                     <$ string "%verbose"
    , PERCENT_YACC                        <$ string "%yacc"
    ]

spaces :: Scanner Token
spaces = undefined

lineComment :: Scanner Token
lineComment = undefined

blockComment :: Scanner Token
blockComment = undefined

lineDirective :: Scanner Token
lineDirective = undefined

colon :: Scanner Token
colon = COLON <$ string ":"

equal :: Scanner Token
equal = EQUAL <$ string "="

pipe :: Scanner Token
pipe = PIPE <$ string "|"

semicolon :: Scanner Token
semicolon = SEMICOLON <$ string ";"

identifier :: Scanner Token
identifier = choice [
      BRACKETED_ID <$> try (id_ <> charT '[' <> id_ <> charT ']' <* lookAhead spaceOrEof)
    , ID_COLON <$> try (id_ <> charT ':' <* lookAhead spaceOrEof)
    , ID <$> try (id_ <* lookAhead spaceOrEof)
    ]

whitespace :: Scanner ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

word :: Scanner Token
word = STRING <$> takeWhile1P (Just "word") (not . isSpace)

token :: Scanner Token
token = choice [
      declarations
    , colon
    , equal
    , pipe
    , semicolon
    , identifier
    , word
    ]

token_ :: Scanner Token
token_ = L.lexeme whitespace token

tokens :: Scanner [Token]
tokens = whitespace >> many token_

scan :: Text -> Maybe [Token]
scan = parseMaybe tokens


