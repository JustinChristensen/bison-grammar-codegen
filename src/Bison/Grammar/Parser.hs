{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Parser where

import Data.Functor
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (Token)
import Bison.Grammar.Types
import Bison.Grammar.Lexer

stringT :: Parser StringT
stringT = makeToken string' <&> StringT

bracedCodeT :: Parser BracedCodeT
bracedCodeT = makeToken bracedCode' <&> BracedCodeT

predicateT :: Parser BracedPredicateT
predicateT =  makeToken predicate' <&> BracedPredicateT

bracketedIdT :: Parser BracketedIdT
bracketedIdT = makeToken bracketedId' <&> BracketedIdT

characterT :: Parser CharT
characterT = makeToken character' <&> CharT

epilogueT :: Parser EpilogueT
epilogueT = makeToken epilogue' <&> EpilogueT

identifierT :: Parser IdT
identifierT = makeToken identifier' <&> IdT

idColonT :: Parser IdColonT
idColonT = makeToken idColon' <&> IdColonT

integerT :: Parser IntT
integerT = makeToken (hexadecimal' <|> decimal') <&> IntT

prologueT :: Parser PrologueT
prologueT = makeToken prologue' <&> PrologueT

pFlagT :: Parser PercentFlagT
pFlagT = makeToken pFlag' <&> PercentFlagT

pParamT :: Parser PercentParamT
pParamT = makeToken pParam'' <&> PercentParamT

tagT :: Parser TagT
tagT = makeToken tag' <&> TagT

grammarFile :: Parser GrammarFile
grammarFile = GrammarFile
    <$> many prologueDecl
    <*> (percentPercent_ *> many grammarRuleOrDecl)
    <*> optional epilogue

prologueDecl :: Parser PrologueDecl
prologueDecl =
        GrammarDeclPD <$> grammarDecl
    <|> ProloguePD <$> prologueT
    <|> FlagPD <$> pFlagT
    <|> DefinePD <$> (pDefine_ *> identifierT) <*> optional value
    <|> DefinesPD <$> (pDefines_ *> optional stringT)
    <|> ErrorVerbosePD <$ pErrorVerbose_
    <|> ExpectPD <$> (pExpect_ *> integerT)
    <|> ExpectRrPD <$> (pExpectRr_ *> integerT)
    <|> FilePrefixPD <$> (pFilePrefix_ *> stringT)
    <|> GlrParserPD <$ pGlrParser_
    <|> InitialActionPD <$> (pInitialAction_ *> bracedCodeT)
    <|> LanguagePD <$> (pLanguage_ *> stringT)
    <|> NamePrefixPD <$> (pNamePrefix_ *> stringT)
    <|> NoLinesPD <$ pNoLines_
    <|> NonDeterministicParserPD <$ pNonDeterministicParser_
    <|> OutputPD <$> (pOutput_ *> stringT)
    <|> ParamPD <$> (pParam_ *> many bracedCodeT)
    <|> PureParserPD <$ pPureParser_
    <|> RequirePD <$> (pRequire_ *> stringT)
    <|> SkeletonPD <$> (pSkeleton_ *> stringT)
    <|> TokenTablePD <$ pTokenTable_
    <|> VerbosePD <$ pVerbose_
    <|> YaccPD <$ pYacc_

grammarDecl :: Parser GrammarDecl
grammarDecl =
        SymbolDeclGD <$> symbolDecl
    <|> StartGD <$> (pStart_ *> symbol)
    <|> DestructorGD <$> (pDestructor_ *> bracedCodeT) <*> some genericSymlistItem
    <|> PrinterGD <$> (pPrinter_ *> bracedCodeT) <*> some genericSymlistItem
    <|> DefaultPrecGD <$ pDefaultPrec_
    <|> NoDefaultPrecGD <$ pNoDefaultPrec_
    <|> CodeGD <$> (pCode_ *> optional identifierT) <*> bracedCodeT
    <|> UnionGD <$> (pUnion_ *> optional identifierT) <*> bracedCodeT

symbolDecl:: Parser SymbolDecl
symbolDecl =
        NTermSD <$> (pNterm_ *> optional tagT) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TokenSD <$> (pToken_ *> optional tagT) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TypeSD <$> (pType_ *> optional tagT) <*> some symbol <*> many (taggedDecls symbol)
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
        LeftPD <$ pLeft_
    <|> RightPD <$ pRight_
    <|> NonAssocPD <$ pNonAssoc_
    <|> PrecedencePD <$ pPrecedence_

genericSymlistItem :: Parser GenericSymlistItem
genericSymlistItem =
        SymbolGS <$> symbol
    <|> TagGS <$> tag

tag :: Parser Tag
tag =
        Tag <$> tagT
    <|> TagAny <$ tagAny_
    <|> TagNone <$ tagNone_

grammarRuleOrDecl :: Parser GrammarRuleOrDecl
grammarRuleOrDecl =
        RuleGD <$> rule
    <|> GrammarDeclGD <$> (grammarDecl <* semicolon_)

rule :: Parser Rule
rule = Rule
    <$> idColonT
    <*> optional bracketedIdT
    <*> (colon_ *> rhses `sepBy1` pipe_ <* semicolon_)

rhses :: Parser Rhses
rhses = Rhses <$> some rhs

rhs :: Parser Rhs
rhs =
        SymR <$> symbol <*> optional bracketedIdT
    <|> TagR <$> optional tagT <*> bracedCodeT <*> optional bracketedIdT
    <|> PredR <$> predicateT
    <|> EmptyR <$ pEmpty_
    <|> PrecR <$> (pPrec_ *> symbol)
    <|> DprecR <$> (pDprec_ *> integerT)
    <|> MergeR <$> (pMerge_ *> tagT)
    <|> ExpectR <$> (pExpect_ *> integerT)
    <|> ExpectRrR <$> (pExpectRr_ *> integerT)

value :: Parser Value
value =
        IdV <$> identifierT
    <|> StrV <$> stringT
    <|> CodeV <$> bracedCodeT

id' :: Parser Id
id' =
        Id <$> identifierT
    <|> Char <$> characterT

symbol :: Parser Symbol
symbol =
        IdS <$> id'
    <|> StrS <$> stringT

epilogue :: Parser Epilogue
epilogue = Epilogue <$> (percentPercent_ *> epilogueT)

