{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Parser where

import Data.Functor
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative hiding (many, some)
import Data.List (intersperse)
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T (singleton, unpack, pack, map)
import Bison.Grammar.Types

whitespace :: Parser ()
whitespace = L.space M.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

makeLexeme :: Parser a -> Parser a
makeLexeme = L.lexeme whitespace

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

percentPercent' :: Parser Text
percentPercent' = makeLexeme $ string "%%"

predicate' :: Parser Text
predicate' = try $ makeLexeme $ string "%?" <> spaces <> bracedCode'

prologue' :: Parser Text
prologue' = try $ makeLexeme $ charT '%' <> bracedCode'

pCode' :: Parser Text
pCode' = makeLexeme $ kw_ "%code"

pDebug' :: Parser Text
pDebug' = makeLexeme $ kw_ "%debug"

pLocations' :: Parser Text
pLocations' = makeLexeme $ kw_ "%locations"

pFlag' :: Parser Text
pFlag' = makeLexeme $ pDebug' <|> pLocations'

pDefaultPrec' :: Parser Text
pDefaultPrec' = makeLexeme $ kwsep_ ["%default", "prec"]

pDefines' :: Parser Text
pDefines' = makeLexeme $ kw_ "%defines"

pDefine' :: Parser Text
pDefine' = makeLexeme $ kw_ "%define"

pDestructor' :: Parser Text
pDestructor' = makeLexeme $ kw_ "%destructor"

pDprec' :: Parser Text
pDprec' = makeLexeme $ kw_ "%dprec"

pEmpty' :: Parser Text
pEmpty' = makeLexeme $ kw_ "%empty"

pErrorVerbose' :: Parser Text
pErrorVerbose' = makeLexeme $ kwsep_ ["%error", "verbose"]

pExpect' :: Parser Text
pExpect' = makeLexeme $ kw_ "%expect"

pExpectRr' :: Parser Text
pExpectRr' = makeLexeme $ kwsep_ ["%expect", "rr"]

pFilePrefix' :: Parser Text
pFilePrefix' = try $ makeLexeme $ kw_ "%file-prefix" <> eqopt_

pInitialAction' :: Parser Text
pInitialAction' = makeLexeme $ kw_ "%initial-action"

pGlrParser' :: Parser Text
pGlrParser' = makeLexeme $ kw_ "%glr-parser"

pLanguage' :: Parser Text
pLanguage' = makeLexeme $ kw_ "%language"

pLeft' :: Parser Text
pLeft' = makeLexeme $ kw_ "%left"

pMerge' :: Parser Text
pMerge' = makeLexeme $ kw_ "%merge"

pNamePrefix' :: Parser Text
pNamePrefix' = try $ makeLexeme $ (kwsep_ ["%name", "prefix"] <> eqopt_)

pNoDefaultPrec' :: Parser Text
pNoDefaultPrec' = makeLexeme $ kwsep_ ["%no", "default", "prec"]

pNoLines' :: Parser Text
pNoLines' = makeLexeme $ kwsep_ ["%no", "lines"]

pBinary' :: Parser Text
pBinary' = makeLexeme $ kw_ "%binary"

pNonAssoc' :: Parser Text
pNonAssoc' = makeLexeme $ kw_ "%nonassoc"

pNonDeterministicParser' :: Parser Text
pNonDeterministicParser' = makeLexeme $ kw_ "%nondeterministic-parser"

pNterm' :: Parser Text
pNterm' = makeLexeme $ kw_ "%nterm"

pOutput' :: Parser Text
pOutput' = try $ makeLexeme $ kw_ "%output" <> eqopt_

pFixedOutputFiles' :: Parser Text
pFixedOutputFiles' = makeLexeme $ kwsep_ ["%fixed", "output", "files"]

pParam' :: Parser Text
pParam' = kw_ "%param"

pLexParam' :: Parser Text
pLexParam' = kw_ "%lex-param"

pParseParam' :: Parser Text
pParseParam' = kw_ "%parse-param"

pParam'' :: Parser Text
pParam'' = makeLexeme $ pParam' <|> pParseParam' <|> pLexParam'

pPrecedence' :: Parser Text
pPrecedence' = makeLexeme $ kw_ "%precedence"

pPrec' :: Parser Text
pPrec' = makeLexeme $ kw_ "%prec"

pPrinter' :: Parser Text
pPrinter' = makeLexeme $ kw_ "%printer"

pPureParser' :: Parser Text
pPureParser' = makeLexeme $ kwsep_ ["%pure", "parser"]

pRequire' :: Parser Text
pRequire' = makeLexeme $ kw_ "%require"

pRight' :: Parser Text
pRight' = makeLexeme $ kw_ "%right"

pSkeleton' :: Parser Text
pSkeleton' = makeLexeme $ kw_ "%skeleton"

pStart' :: Parser Text
pStart' = makeLexeme $ kw_ "%start"

pTerm' :: Parser Text
pTerm' = makeLexeme $ kw_ "%term"

pTokenTable' :: Parser Text
pTokenTable' = makeLexeme $ kwsep_ ["%token", "table"]

pToken' :: Parser Text
pToken' = makeLexeme $ kw_ "%token"

pType' :: Parser Text
pType' = makeLexeme $ kw_ "%type"

pUnion' :: Parser Text
pUnion' = makeLexeme $ kw_ "%union"

pVerbose' :: Parser Text
pVerbose' = makeLexeme $ kw_ "%verbose"

pYacc' :: Parser Text
pYacc' = makeLexeme $ kw_ "%yacc"

colon' :: Parser Text
colon' = makeLexeme (charT ':')

equal' :: Parser Text
equal' = makeLexeme (charT '=')

pipe' :: Parser Text
pipe' = makeLexeme (charT '|')

semicolon' :: Parser Text
semicolon' = makeLexeme (charT ';')

identifier' :: Parser Text
identifier' = try $ makeLexeme $ id_ <* notFollowedBy (letter_ <|> charT ':')

idColon' :: Parser Text
idColon' = try $ makeLexeme $ id_ <* spaces <* lookAhead (test ':' <|> bracketedId' *> test ':')

bracketedId' :: Parser Text
bracketedId' = try $ makeLexeme $ option "" id_ <> charT '[' <> id_ <> charT ']'

hexadecimal' :: Parser Int
hexadecimal' = try $ makeLexeme $ M.char '0' >> satisfy (`elem` ['x', 'X']) >>
    L.hexadecimal <* notFollowedBy letter_

decimal' :: Parser Int
decimal' = try $ makeLexeme $ L.decimal <* notFollowedBy letter_

character' :: Parser Char
character' = try $ makeLexeme $ M.char '\'' >> L.charLiteral <* M.char '\''

string'' :: Parser Text
string'' = M.char '"' >> (T.pack <$> manyTill L.charLiteral (M.char '"'))

string' :: Parser Text
string' = try $ makeLexeme string''

bracedCode' :: Parser Text
bracedCode' = try $ makeLexeme $ nested "{" "}" p <* notFollowedBy (M.char '}')
    where
        wrap c str = let c' = T.singleton c
                    in pure $ c' <> str <> c'
        p = nested "{" "}" p
            <|> (string'' >>= wrap '"')
            <|> (T.singleton <$> character' >>= wrap '\'')
            <|> nested "/*" "*/" anySingleT
            <|> lineWithPrefix "//"
            <|> anySingleT

tagAny' :: Parser Text
tagAny' = makeLexeme $ string "<*>"

tagNone' :: Parser Text
tagNone' = makeLexeme $ string "<>"

tag' :: Parser Text
tag' = try $ makeLexeme $ nested "<" ">" p <* notFollowedBy (M.char '>')
    where
        p = nested "<" ">" p
            <|> string "->"
            <|> satisfyT (`notElem` ['<', '>'])

epilogue' :: Parser Text
epilogue' = takeRest <* eof

stringT :: Parser StringT
stringT = string' <&> StringT

bracedCodeT :: Parser BracedCodeT
bracedCodeT = bracedCode' <&> BracedCodeT

predicateT :: Parser BracedPredicateT
predicateT =  predicate' <&> BracedPredicateT

bracketedIdT :: Parser BracketedIdT
bracketedIdT = bracketedId' <&> BracketedIdT

characterT :: Parser CharT
characterT = character' <&> CharT

epilogueT :: Parser EpilogueT
epilogueT = epilogue' <&> EpilogueT

identifierT :: Parser IdT
identifierT = identifier' <&> IdT

idColonT :: Parser IdColonT
idColonT = idColon' <&> IdColonT

integerT :: Parser IntT
integerT = (hexadecimal' <|> decimal') <&> IntT

prologueT :: Parser PrologueT
prologueT = prologue' <&> PrologueT

pFlagT :: Parser PercentFlagT
pFlagT = pFlag' <&> PercentFlagT

pParamT :: Parser PercentParamT
pParamT = pParam'' <&> PercentParamT

tagT :: Parser TagT
tagT = tag' <&> TagT

grammarFile :: Parser GrammarFile
grammarFile = GrammarFile
    <$> (whitespace *> manyTill prologueDecl percentPercent')
    <*> (manyTill grammarRuleOrDecl (eof <|> void percentPercent'))
    <*> optional epilogue

prologueDecl :: Parser PrologueDecl
prologueDecl =
        GrammarDeclPD <$> (grammarDecl <* optional semicolon')
    <|> ProloguePD <$> prologueT
    <|> FlagPD <$> pFlagT
    <|> DefinePD <$> (pDefine' *> identifierT) <*> optional value
    <|> DefinesPD <$> (pDefines' *> optional stringT)
    <|> ErrorVerbosePD <$ pErrorVerbose'
    <|> ExpectPD <$> (pExpect' *> integerT)
    <|> ExpectRrPD <$> (pExpectRr' *> integerT)
    <|> FilePrefixPD <$> (pFilePrefix' *> stringT)
    <|> GlrParserPD <$ pGlrParser'
    <|> InitialActionPD <$> (pInitialAction' *> bracedCodeT)
    <|> LanguagePD <$> (pLanguage' *> stringT)
    <|> NamePrefixPD <$> (pNamePrefix' *> stringT)
    <|> NoLinesPD <$ pNoLines'
    <|> NonDeterministicParserPD <$ pNonDeterministicParser'
    <|> OutputPD <$> (pOutput' *> stringT)
    <|> ParamPD <$> pParamT <*> (many bracedCodeT)
    <|> PureParserPD <$ pPureParser'
    <|> RequirePD <$> (pRequire' *> stringT)
    <|> SkeletonPD <$> (pSkeleton' *> stringT)
    <|> TokenTablePD <$ pTokenTable'
    <|> VerbosePD <$ pVerbose'
    <|> YaccPD <$ pYacc'

grammarDecl :: Parser GrammarDecl
grammarDecl =
        SymbolDeclGD <$> symbolDecl
    <|> StartGD <$> (pStart' *> symbol)
    <|> DestructorGD <$> (pDestructor' *> bracedCodeT) <*> some genericSymlistItem
    <|> PrinterGD <$> (pPrinter' *> bracedCodeT) <*> some genericSymlistItem
    <|> DefaultPrecGD <$ pDefaultPrec'
    <|> NoDefaultPrecGD <$ pNoDefaultPrec'
    <|> CodeGD <$> (pCode' *> optional identifierT) <*> bracedCodeT
    <|> UnionGD <$> (pUnion' *> optional identifierT) <*> bracedCodeT

symbolDecl:: Parser SymbolDecl
symbolDecl =
        NTermSD <$> (pNterm' *> optional tagT) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TokenSD <$> (pToken' *> optional tagT) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TypeSD <$> (pType' *> optional tagT) <*> some symbol <*> many (taggedDecls symbol)
    <|> PrecedenceDeclSD <$> precedenceDecl <*> optional tagT <*> some precTokenDecl <*> many (taggedDecls precTokenDecl)

taggedDecls :: Parser a -> Parser (TaggedDecls a)
taggedDecls p = TaggedDecls <$> tagT <*> some p

tokenDecl :: Parser TokenDecl
tokenDecl = TokenDecl <$> id' <*> optional integerT <*> optional stringT

precTokenDecl :: Parser PrecTokenDecl
precTokenDecl =
        IdP <$> id' <*> optional integerT
    <|> StrP <$> stringT

precedenceDecl :: Parser PrecedenceDecl
precedenceDecl =
        LeftPD <$ pLeft'
    <|> RightPD <$ pRight'
    <|> NonAssocPD <$ pNonAssoc'
    <|> PrecedencePD <$ pPrecedence'

genericSymlistItem :: Parser GenericSymlistItem
genericSymlistItem =
        SymbolGS <$> symbol
    <|> TagGS <$> tag

tag :: Parser Tag
tag =
        Tag <$> tagT
    <|> TagAny <$ tagAny'
    <|> TagNone <$ tagNone'

grammarRuleOrDecl :: Parser GrammarRuleOrDecl
grammarRuleOrDecl =
        RuleGD <$> rule
    <|> GrammarDeclGD <$> (grammarDecl <* semicolon')

rule :: Parser Rule
rule = Rule
    <$> idColonT
    <*> optional bracketedIdT
    <*> (colon' *> rhses `sepBy1` pipe' <* optional semicolon')

rhses :: Parser Rhses
rhses = Rhses <$> (([EmptyR] <$ lookAhead (pipe' <|> semicolon')) <|> some rhs)

rhs :: Parser Rhs
rhs =
        SymR <$> symbol <*> optional bracketedIdT
    <|> TagR <$> optional tagT <*> bracedCodeT <*> optional bracketedIdT
    <|> PredR <$> predicateT
    <|> EmptyR <$ pEmpty'
    <|> PrecR <$> (pPrec' *> symbol)
    <|> DprecR <$> (pDprec' *> integerT)
    <|> MergeR <$> (pMerge' *> tagT)
    <|> ExpectRrR <$> (pExpectRr' *> integerT)
    <|> ExpectR <$> (pExpect' *> integerT)

value :: Parser ValueN
value =
        IdV <$> identifierT
    <|> StrV <$> stringT
    <|> CodeV <$> bracedCodeT

id' :: Parser IdN
id' =
        Id <$> identifierT
    <|> Char <$> characterT

symbol :: Parser SymbolN
symbol =
        IdS <$> id'
    <|> StrS <$> stringT

epilogue :: Parser Epilogue
epilogue = Epilogue <$> epilogueT

