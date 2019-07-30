{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Control.Applicative
import Text.Megaparsec.Char
import Data.Text (Text)
import Bison.Grammar.Types

symbol :: Text -> TokenType -> Scanner Token
symbol s tokType = do
    text <- string s
    pure (tokType, text)

declarations :: Scanner Token
declarations =
        symbol "%binary"                  PERCENT_NONASSOC
    <|> symbol "%code"                    PERCENT_CODE
    <|> symbol "%debug"                   PERCENT_FLAG
    <|> symbol "%default-prec"            PERCENT_DEFAULT_PREC
    <|> symbol "%define"                  PERCENT_DEFINE
    <|> symbol "%defines"                 PERCENT_DEFINES
    <|> symbol "%destructor"              PERCENT_DESTRUCTOR
    <|> symbol "%dprec"                   PERCENT_DPREC
    <|> symbol "%empty"                   PERCENT_EMPTY
    <|> symbol "%expect"                  PERCENT_EXPECT
    <|> symbol "%expect-rr"               PERCENT_EXPECT_RR
    <|> symbol "%file-prefix"             PERCENT_FILE_PREFIX
    <|> symbol "%initial-action"          PERCENT_INITIAL_ACTION
    <|> symbol "%glr-parser"              PERCENT_GLR_PARSER
    <|> symbol "%language"                PERCENT_LANGUAGE
    <|> symbol "%left"                    PERCENT_LEFT
    <|> symbol "%lex-param"               PERCENT_PARAM
    <|> symbol "%locations"               PERCENT_FLAG
    <|> symbol "%merge"                   PERCENT_MERGE
    <|> symbol "%no-default-prec"         PERCENT_NO_DEFAULT_PREC
    <|> symbol "%no-lines"                PERCENT_NO_LINES
    <|> symbol "%nonassoc"                PERCENT_NONASSOC
    <|> symbol "%nondeterministic-parser" PERCENT_NONDETERMINISTIC_PARSER
    <|> symbol "%nterm"                   PERCENT_NTERM
    <|> symbol "%output"                  PERCENT_OUTPUT
    <|> symbol "%param"                   PERCENT_PARAM
    <|> symbol "%parse-param"             PERCENT_PARAM
    <|> symbol "%prec"                    PERCENT_PREC
    <|> symbol "%precedence"              PERCENT_PRECEDENCE
    <|> symbol "%printer"                 PERCENT_PRINTER
    <|> symbol "%require"                 PERCENT_REQUIRE
    <|> symbol "%right"                   PERCENT_RIGHT
    <|> symbol "%skeleton"                PERCENT_SKELETON
    <|> symbol "%start"                   PERCENT_START
    <|> symbol "%term"                    PERCENT_TOKEN
    <|> symbol "%token"                   PERCENT_TOKEN
    <|> symbol "%token-table"             PERCENT_TOKEN_TABLE
    <|> symbol "%type"                    PERCENT_TYPE
    <|> symbol "%union"                   PERCENT_UNION
    <|> symbol "%verbose"                 PERCENT_VERBOSE
    <|> symbol "%yacc"                    PERCENT_YACC

deprecated :: Scanner Token
deprecated = undefined

spaces :: Scanner Token
spaces = undefined

lineComment :: Scanner Token
lineComment = undefined

blockComment :: Scanner Token
blockComment = undefined

lineDirective :: Scanner Token
lineDirective = undefined

whitespace :: Scanner Token
whitespace =
        spaces
    <|> lineComment
    <|> blockComment
    <|> lineDirective

colon :: Scanner Token
colon = symbol ":" COLON

equal :: Scanner Token
equal = symbol "=" EQUAL

pipe :: Scanner Token
pipe = symbol "|" PIPE

semicolon :: Scanner Token
semicolon = symbol ";" SEMICOLON

percentId :: Scanner Token
percentId = undefined

-- token :: ScanState -> (ScanState, Scanner Token)
-- token ss = case state of
--     INITIAL ->
--             whitespace
--         <|> declarations
--         <|> deprecated
--         <|> percentId -- error
--         <|> colon
--         <|> equal
--         <|> pipe
--         <|> semicolon
--     SC_YACC_COMMENT ->
--     SC_ESCAPED_STRING ->
--     SC_ESCAPED_CHARACTER ->
--     SC_AFTER_IDENTIFIER ->
--             whitespace
--     SC_TAG ->
--     SC_PROLOGUE ->
--     SC_BRACED_CODE ->
--     SC_EPILOGUE ->
--     SC_PREDICATE ->
--     SC_COMMENT ->
--     SC_LINE_COMMENT ->
--     SC_STRING ->
--     SC_CHARACTER ->
--     SC_BRACKETED_ID ->
--             whitespace
--     SC_TAG ->
--     SC_RETURN_BRACKETED_ID ->
--             whitespace

scan :: Scanner [Token]
scan = undefined



