{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intersperse)
import Control.Applicative hiding (many, some)
import Control.Monad.State (get, put, gets)
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

spaces :: Scanner Text
spaces = takeWhileP (Just "white space") isSpace

eqopt_ :: Scanner Text
eqopt_ = option "" (spaces *> charT '=')

sep_ :: [Char] -> [Text] -> Scanner Text
sep_ seps strs = T.map dashes <$> (try $ mconcat $
        intersperse (satisfyT (`elem` seps)) (string <$> strs))
    where
        dashes '_' = '-'
        dashes c = c

directive_ :: Text -> Scanner Text
directive_ d = string d <* notFollowedBy letter_

declarations :: Scanner Token
declarations = let seps = "-_" in choice
    [
      PERCENT_NONASSOC                    <$ directive_ "binary"
    , PERCENT_CODE                        <$ directive_ "code"
    , PERCENT_FLAG                        <$> directive_ "debug"
    , PERCENT_DEFAULT_PREC                <$ sep_ seps ["default", "prec"]
    , PERCENT_DEFINES                     <$ directive_ "defines"
    , PERCENT_DEFINE                      <$ directive_ "define"
    , PERCENT_DESTRUCTOR                  <$ directive_ "destructor"
    , PERCENT_DPREC                       <$ directive_ "dprec"
    , PERCENT_EMPTY                       <$ directive_ "empty"
    , PERCENT_ERROR_VERBOSE               <$ sep_ seps ["error", "verbose"] -- deprecated
    , PERCENT_EXPECT                      <$ directive_ "expect"
    , PERCENT_EXPECT_RR                   <$ sep_ seps ["expect", "rr"] -- deprecated
    , PERCENT_FILE_PREFIX                 <$ try (string "file-prefix" <> eqopt_) -- deprecated
    , PERCENT_INITIAL_ACTION              <$ directive_ "initial-action"
    , PERCENT_GLR_PARSER                  <$ directive_ "glr-parser"
    , PERCENT_LANGUAGE                    <$ directive_ "language"
    , PERCENT_LEFT                        <$ directive_ "left"
    , PERCENT_PARAM                       <$ directive_ "lex-param"
    , PERCENT_FLAG                        <$> directive_ "locations"
    , PERCENT_MERGE                       <$ directive_ "merge"
    , PERCENT_NAME_PREFIX                 <$ try (sep_ seps ["name", "prefix"] <> eqopt_)  -- deprecated
    , PERCENT_NO_DEFAULT_PREC             <$ sep_ seps ["no", "default", "prec"] -- deprecated
    , PERCENT_NO_LINES                    <$ sep_ seps ["no", "lines"] -- deprecated
    , PERCENT_NONASSOC                    <$ directive_ "nonassoc"
    , PERCENT_NONDETERMINISTIC_PARSER     <$ directive_ "nondeterministic-parser"
    , PERCENT_NTERM                       <$ directive_ "nterm"
    , PERCENT_OUTPUT                      <$ try (string "output" <> eqopt_) -- deprecated
    , PERCENT_FIXED_OUTPUT_FILES          <$ sep_ seps ["fixed", "output", "files"] -- deprecated
    , PERCENT_PARAM                       <$ directive_ "param"
    , PERCENT_PARAM                       <$ directive_ "parse-param"
    , PERCENT_PREC                        <$ directive_ "prec"
    , PERCENT_PRECEDENCE                  <$ directive_ "precedence"
    , PERCENT_PRINTER                     <$ directive_ "printer"
    , PERCENT_PURE_PARSER                 <$ sep_ seps ["pure", "parser"] -- deprecated
    , PERCENT_REQUIRE                     <$ directive_"require"
    , PERCENT_RIGHT                       <$ directive_"right"
    , PERCENT_SKELETON                    <$ directive_"skeleton"
    , PERCENT_START                       <$ directive_"start"
    , PERCENT_TOKEN                       <$ directive_"term"
    , PERCENT_TOKEN_TABLE                 <$ sep_ seps ["token", "table"] -- deprecated
    , PERCENT_TOKEN                       <$ directive_"token"
    , PERCENT_TYPE                        <$ directive_ "type"
    , PERCENT_UNION                       <$ directive_ "union"
    , PERCENT_VERBOSE                     <$ directive_ "verbose"
    , PERCENT_YACC                        <$ directive_ "yacc"
    ]

colon :: Scanner Token
colon = COLON <$ charT ':'

equal :: Scanner Token
equal = EQUAL <$ charT '='

pipe :: Scanner Token
pipe = PIPE <$ charT '|'

semicolon :: Scanner Token
semicolon = SEMICOLON <$ charT ';'

bracketedId_ :: Text -> Scanner Token
bracketedId_ pre = try $ BRACKETED_ID <$> ((pre <>) <$> id_ <> charT ']')

identifier_ :: Text -> Scanner Token
identifier_ pre = choice [
      try $ (pre <>) <$> charT '[' >>= bracketedId_
    , pure (ID_COLON pre) <* lookAhead (charT ':')
    , pure (ID pre) <* lookAhead spaceOrEof
    ]

percentPercent_ :: Scanner Token
percentPercent_ = do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

percent_ :: Text -> Scanner Token
percent_ pre = choice [
      try $ charT '%' >> percentPercent_
    , try $ (pre <>) <$> charT '{' >>= prologue_
    , try $ (pre <>) <$> charT '?' <> spaces <> charT '{' >>= predicate_
    , declarations
    ]

predicate_ :: Text -> Scanner Token
predicate_ pre = undefined

prologue_ :: Text -> Scanner Token
prologue_ pre = undefined

initial_ :: Scanner Token
initial_ = do
    sect <- gets section
    case sect of
        Epilogue -> EPILOGUE <$> takeRest <* eof
        _ -> choice [
              try $ charT '%' >>= percent_
            , colon
            , equal
            , pipe
            , semicolon
            , try $ id_ >>= identifier_
            , INT <$> L.decimal
            , INT <$> L.hexadecimal
            , try $ charT '[' >>= bracketedId_
            , word
            ]

whitespace :: Scanner ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

word :: Scanner Token
word = STRING <$> takeWhile1P (Just "word") (not . isSpace)

token :: Scanner Token
token = L.lexeme whitespace initial_

tokens :: Scanner [Token]
tokens = whitespace >> manyTill token eof

scan :: Text -> Maybe [Token]
scan = parseMaybe (runScanner initialState tokens)
    where initialState = ScanState Prologue 0



