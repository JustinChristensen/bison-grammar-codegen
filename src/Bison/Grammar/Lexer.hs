{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Lexer where

import Data.Functor
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intersperse)
import Control.Applicative hiding (many, some)
import Control.Monad.State (get, put, gets)
import Text.Megaparsec hiding (Token, token, tokens)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T (singleton, unpack, pack, map)
import Bison.Grammar.Types

test :: Char -> Scanner Char
test = lookAhead . M.char

spaceOrEof :: Scanner ()
spaceOrEof = (() <$ M.spaceChar) <|> eof

satisfyT :: (Char -> Bool) -> Scanner Text
satisfyT f = T.singleton <$> satisfy f

anySingleT :: Scanner Text
anySingleT = T.singleton <$> anySingle

charT :: Char -> Scanner Text
charT c = T.singleton <$> M.char c

isLetter :: Char -> Bool
isLetter c = isAlpha c || c `elem` ['.', '_']

isLetterOrNum :: Char -> Bool
isLetterOrNum c = isLetter c || c == '-' || isDigit c

letter_ :: Scanner Text
letter_ = satisfyT isLetter <?> "letter, '.', or '_'"

letterOrNum_ :: Scanner Text
letterOrNum_ = satisfyT isLetterOrNum <?> "alphanumeric character, '.', or '_'"

id_ :: Scanner Text
id_ = letter_ <>
    takeWhileP (Just "alphanumeric character, '.', or '_'") isLetterOrNum

spaces :: Scanner Text
spaces = takeWhileP (Just "white space") isSpace

eqopt :: Scanner Text
eqopt = option "" (spaces *> charT '=')

lineWithPrefix :: Text -> Scanner Text
lineWithPrefix p = string p <> takeWhileP (Just "character") (/= '\n')

nested :: Text -> Text -> Scanner Text -> Scanner Text
nested start end content =
        start' <> (mconcat <$> manyTill content (lookAhead end')) <> end'
    where
        start' = string start
        end' = string end

codeBlock :: Scanner Text
codeBlock = nested "{" "}" codeBlock
        <|> (stringLit >>= wrap '"')
        <|> (T.singleton <$> characterLit >>= wrap '\'')
        <|> nested "/*" "*/" anySingleT
        <|> lineWithPrefix "//"
        <|> anySingleT
    where
        wrap c str = let c' = T.singleton c
                    in pure $ c' <> str <> c'

tag :: Scanner Text
tag = nested "<" ">" (try tag <|> try (string "->") <|> satisfyT (`notElem` ['<', '>']))

sep_ :: [Char] -> [Text] -> Scanner Text
sep_ seps strs = (T.map dashes <$> (try $ mconcat $
        intersperse (satisfyT (`elem` seps)) (string <$> strs)))
        <?> T.unpack (mconcat $ intersperse "-" strs)
    where
        dashes '_' = '-'
        dashes c = c

directive_ :: Text -> Scanner Text
directive_ d = string d <* notFollowedBy letter_

declarations :: Scanner Token
declarations = let seps = "-_" in choice
    [
      directive_ "binary"                                         $> PERCENT_NONASSOC
    , directive_ "code"                                           $> PERCENT_CODE
    , directive_ "debug"                                         <&> PERCENT_FLAG
    , sep_ seps ["default", "prec"]                               $> PERCENT_DEFAULT_PREC
    , directive_ "defines"                                        $> PERCENT_DEFINES
    , directive_ "define"                                         $> PERCENT_DEFINE
    , directive_ "destructor"                                     $> PERCENT_DESTRUCTOR
    , directive_ "dprec"                                          $> PERCENT_DPREC
    , directive_ "empty"                                          $> PERCENT_EMPTY
    , sep_ seps ["error", "verbose"]                              $> PERCENT_ERROR_VERBOSE -- deprecated
    , directive_ "expect"                                         $> PERCENT_EXPECT
    , sep_ seps ["expect", "rr"]                                  $> PERCENT_EXPECT_RR -- deprecated
    , try (string "file-prefix" <> eqopt)                         $> PERCENT_FILE_PREFIX -- deprecated
    , directive_ "initial-action"                                 $> PERCENT_INITIAL_ACTION
    , directive_ "glr-parser"                                     $> PERCENT_GLR_PARSER
    , directive_ "language"                                       $> PERCENT_LANGUAGE
    , directive_ "left"                                           $> PERCENT_LEFT
    , directive_ "lex-param"                                      $> PERCENT_PARAM
    , directive_ "locations"                                     <&> PERCENT_FLAG
    , directive_ "merge"                                          $> PERCENT_MERGE
    , try (sep_ seps ["name", "prefix"] <> eqopt)                 $> PERCENT_NAME_PREFIX -- deprecated
    , sep_ seps ["no", "default", "prec"]                         $> PERCENT_NO_DEFAULT_PREC -- deprecated
    , sep_ seps ["no", "lines"]                                   $> PERCENT_NO_LINES -- deprecated
    , directive_ "nonassoc"                                       $> PERCENT_NONASSOC
    , directive_ "nondeterministic-parser"                        $> PERCENT_NONDETERMINISTIC_PARSER
    , directive_ "nterm"                                          $> PERCENT_NTERM
    , try (string "output" <> eqopt)                              $> PERCENT_OUTPUT -- deprecated
    , sep_ seps ["fixed", "output", "files"]                      $> PERCENT_FIXED_OUTPUT_FILES -- deprecated
    , directive_ "param"                                          $> PERCENT_PARAM
    , directive_ "parse-param"                                    $> PERCENT_PARAM
    , directive_ "prec"                                           $> PERCENT_PREC
    , directive_ "precedence"                                     $> PERCENT_PRECEDENCE
    , directive_ "printer"                                        $> PERCENT_PRINTER
    , sep_ seps ["pure", "parser"]                                $> PERCENT_PURE_PARSER -- deprecated
    , directive_"require"                                         $> PERCENT_REQUIRE
    , directive_"right"                                           $> PERCENT_RIGHT
    , directive_"skeleton"                                        $> PERCENT_SKELETON
    , directive_"start"                                           $> PERCENT_START
    , directive_"term"                                            $> PERCENT_TOKEN
    , sep_ seps ["token", "table"]                                $> PERCENT_TOKEN_TABLE -- deprecated
    , directive_"token"                                           $> PERCENT_TOKEN
    , directive_ "type"                                           $> PERCENT_TYPE
    , directive_ "union"                                          $> PERCENT_UNION
    , directive_ "verbose"                                        $> PERCENT_VERBOSE
    , directive_ "yacc"                                           $> PERCENT_YACC
    ]

colon :: Scanner Token
colon = charT ':' $> COLON

equal :: Scanner Token
equal = charT '=' $> EQUAL

pipe :: Scanner Token
pipe = charT '|' $> PIPE

semicolon :: Scanner Token
semicolon = charT ';' $> SEMICOLON

bracketedId :: Text -> Scanner Token
bracketedId pre = try $ ((pre <>) <$> id_ <> charT ']') <&> BRACKETED_ID

identifier :: Text -> Scanner Token
identifier pre = choice [
      try $ (pre <>) <$> charT '[' >>= bracketedId
    , test ':' $> ID_COLON pre
    , ID pre <$ notFollowedBy letter_
    ]

percentPercent :: Scanner Token
percentPercent = do
    ss <- get
    put ss { section = succ (section ss) }
    pure PERCENT_PERCENT

percent :: Text -> Scanner Token
percent pre = choice [
      try $ charT '%' >> percentPercent
    , try $ prologue pre
    , try $ predicate pre
    , declarations
    ]

predicate :: Text -> Scanner Token
predicate pre =  (pre <>) <$> charT '?' <> spaces <> (test '{' >> codeBlock) <&> BRACED_PREDICATE

prologue :: Text -> Scanner Token
prologue pre = (pre <>) <$> (test '{' >> codeBlock) <&> PROLOGUE

characterLit :: Scanner Char
characterLit = M.char '\'' >> L.charLiteral <* M.char '\''

stringLit :: Scanner Text
stringLit = M.char '"' >> (T.pack <$> manyTill L.charLiteral (M.char '"'))

decimal :: Scanner Token
decimal = L.decimal <* notFollowedBy letter_ <&> INT

hexadecimal :: Scanner Token
hexadecimal = M.char '0' >> satisfy (`elem` ['x', 'X']) >>
    L.hexadecimal <* notFollowedBy letter_ <&> INT

word :: Scanner Token
word = do
    sect <- gets section
    case sect of
        Epilogue -> EPILOGUE <$> takeRest <* eof
        _ -> choice [
              try $ charT '%' >>= percent
            , colon
            , equal
            , pipe
            , semicolon
            , try $ id_ >>= identifier
            , try decimal
            , try hexadecimal
            , try characterLit <&> CHAR
            , try stringLit <&> STRING
            , test '{' >> try codeBlock <&> BRACED_CODE
            , string "<*>" $> TAG_ANY
            , string "<>" $> TAG_NONE
            , test '<' >> try tag <&> TAG
            , try $ charT '[' >>= bracketedId
            ]

whitespace :: Scanner ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

token :: Scanner Token
token = L.lexeme whitespace word

tokens :: Scanner [Token]
tokens = whitespace >> manyTill token eof

scan :: Text -> Maybe [Token]
scan = parseMaybe (runScanner initialState tokens)
    where initialState = ScanState Prologue

