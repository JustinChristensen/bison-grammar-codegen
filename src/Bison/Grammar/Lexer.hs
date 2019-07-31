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

satisfyT :: (Char -> Bool) -> Scanner Text
satisfyT f = T.singleton <$> satisfy f

charT :: Char -> Scanner Text
charT c = T.singleton <$> M.char c

isLetter :: Char -> Bool
isLetter c = isAlpha c || c `elem` ['.', '_']

isLetterOrNum :: Char -> Bool
isLetterOrNum c = isLetter c || c == '-' || isDigit c

letter_ :: Scanner Text
letter_ = satisfyT isLetter

id_ :: Scanner Text
id_ = letter_ <> takeWhileP Nothing isLetterOrNum

int_ :: Scanner Text
int_ = takeWhile1P Nothing isDigit

xint_ :: Scanner Text
xint_ = (charT '0')
        <> (satisfyT (`elem` ['x', 'X']))
        <> takeWhile1P Nothing isHexDigit

splice_ :: Scanner Text
splice_ = undefined

sp_ :: Scanner Text
sp_ = takeWhileP Nothing (isSpace)

eqopt_ :: Scanner Text
eqopt_ = option "" (sp_ <> charT '=')

infixr 7 &#
(&#) :: Scanner Text -> TokenType -> Scanner Token
s &# t = (,) t <$> s

sep_ :: [Char] -> [Text] -> Scanner Text
sep_ seps strs = T.map dashes <$> (try $ mconcat $
        intersperse (satisfyT (`elem` seps)) (string <$> strs))
    where
        dashes '_' = '-'
        dashes c = c

declarations :: Scanner Token
declarations = let seps = "-_" in choice
    [
      string "%binary"                           &# PERCENT_NONASSOC
    , string "%code"                             &# PERCENT_CODE
    , string "%debug"                            &# PERCENT_FLAG
    , sep_ seps ["%default", "prec"]             &# PERCENT_DEFAULT_PREC
    , string "%defines"                          &# PERCENT_DEFINES
    , string "%define"                           &# PERCENT_DEFINE
    , string "%destructor"                       &# PERCENT_DESTRUCTOR
    , string "%dprec"                            &# PERCENT_DPREC
    , string "%empty"                            &# PERCENT_EMPTY
    , sep_ seps ["%error", "verbose"]            &# PERCENT_ERROR_VERBOSE -- deprecated
    , string "%expect"                           &# PERCENT_EXPECT
    , sep_ seps ["%expect", "rr"]                &# PERCENT_EXPECT_RR -- deprecated
    , try (string "%file-prefix" <> eqopt_)      &# PERCENT_FILE_PREFIX -- deprecated
    , string "%initial-action"                   &# PERCENT_INITIAL_ACTION
    , string "%glr-parser"                       &# PERCENT_GLR_PARSER
    , string "%language"                         &# PERCENT_LANGUAGE
    , string "%left"                             &# PERCENT_LEFT
    , string "%lex-param"                        &# PERCENT_PARAM
    , string "%locations"                        &# PERCENT_FLAG
    , string "%merge"                            &# PERCENT_MERGE
    , try (sep_ seps ["%name", "prefix"]
        <> eqopt_ <> sp_)                        &# PERCENT_NAME_PREFIX -- deprecated
    , sep_ seps ["%no", "default", "prec"]       &# PERCENT_NO_DEFAULT_PREC -- deprecated
    , sep_ seps ["%no", "lines"]                 &# PERCENT_NO_LINES -- deprecated
    , string "%nonassoc"                         &# PERCENT_NONASSOC
    , string "%nondeterministic-parser"          &# PERCENT_NONDETERMINISTIC_PARSER
    , string "%nterm"                            &# PERCENT_NTERM
    , try (string "%output" <> eqopt_)           &# PERCENT_OUTPUT -- deprecated
    -- , sep_ seps ["%fixed", "output", "files"]       DEPRECATED ("%output \"y.tab.c\"");
    , string "%param"                            &# PERCENT_PARAM
    , string "%parse-param"                      &# PERCENT_PARAM
    , string "%prec"                             &# PERCENT_PREC
    , string "%precedence"                       &# PERCENT_PRECEDENCE
    , string "%printer"                          &# PERCENT_PRINTER
    , sep_ seps ["%pure", "parser"]              &# PERCENT_PURE_PARSER -- deprecated
    , string "%require"                          &# PERCENT_REQUIRE
    , string "%right"                            &# PERCENT_RIGHT
    , string "%skeleton"                         &# PERCENT_SKELETON
    , string "%start"                            &# PERCENT_START
    , string "%term"                             &# PERCENT_TOKEN
    , string "%token"                            &# PERCENT_TOKEN
    , sep_ seps ["%token", "table"]              &# PERCENT_TOKEN_TABLE -- deprecated
    , string "%type"                             &# PERCENT_TYPE
    , string "%union"                            &# PERCENT_UNION
    , string "%verbose"                          &# PERCENT_VERBOSE
    , string "%yacc"                             &# PERCENT_YACC
    ]

spaces :: Scanner Token
spaces = undefined

lineComment :: Scanner Token
lineComment = undefined

blockComment :: Scanner Token
blockComment = undefined

lineDirective :: Scanner Token
lineDirective = undefined

-- whitespace :: Scanner Token
-- whitespace =
--         spaces
--     <|> lineComment
--     <|> blockComment <> scan SC_YACC_COMMENT
--     <|> lineDirective

colon :: Scanner Token
colon = string ":" &# COLON

equal :: Scanner Token
equal = string "=" &# EQUAL

pipe :: Scanner Token
pipe = string "|" &# PIPE

semicolon :: Scanner Token
semicolon = string ";"  &# SEMICOLON

whitespace :: Scanner ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

word :: Scanner Text
word = takeWhile1P Nothing (not . isSpace)

token :: Scanner Token
token = L.lexeme whitespace (word &# STRING)

tokens :: Scanner [Token]
tokens = many token

scan :: Text -> Maybe [Token]
scan = parseMaybe tokens




