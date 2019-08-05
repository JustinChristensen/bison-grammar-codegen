{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Parser where

import Prelude hiding (id)
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (Token)
import Bison.Grammar.Types
import Bison.Grammar.Lexer

grammarFile :: Parser GrammarFile
grammarFile = GrammarFile
    <$> prologueDecls
    <*> percentPercent_
    <*> grammar
    <*> optEpilogue

prologueDecls :: Parser [PrologueDecl]
prologueDecls = many prologueDeclaration

prologueDecl :: Parser PrologueDecl
prologueDecl =
        PrologueDecl <$> grammarDeclaration
    <|> PrologueDecl <$> prologue_
    <|> PrologueDecl <$> pFlag_
    <|> PrologueDecl <$> pDefine_ <*> identifier_ <*> value
    <|> PrologueDecl <$> pDefines_
    <|> PrologueDecl <$> pDefines_ <*> string_
    <|> PrologueDecl <$> pErrorVerbose_
    <|> PrologueDecl <$> pExpect_ <*> integer_
    <|> PrologueDecl <$> pExpectRr_ <*> integer_
    <|> PrologueDecl <$> pFilePrefix_ <*> string_
    <|> PrologueDecl <$> pGlrParser_
    <|> PrologueDecl <$> pInitialAction_
    <|> PrologueDecl <$> pLanguage_ <*> string_
    <|> PrologueDecl <$> pNamePrefix_ <*> string_
    <|> PrologueDecl <$> pNoLines_
    <|> PrologueDecl <$> pNonDeterministicParser_
    <|> PrologueDecl <$> pOutput_ <*> string_
    <|> PrologueDecl <$> pParam_ <*> many bracedCode_
    <|> PrologueDecl <$> pPureParser_
    <|> PrologueDecl <$> pRequire_ <*> string_
    <|> PrologueDecl <$> pSkeleton_ <*> string_
    <|> PrologueDecl <$> pTokenTable_
    <|> PrologueDecl <$> pVerbose_
    <|> PrologueDecl <$> pYacc_

grammarDeclaration :: Parser GrammarDecl
grammarDeclaration =
        GrammarDecl <$> symbolDeclaration
    <|> GrammarDecl <$> pStart_ <*> symbol
    <|> GrammarDecl <$> codePropsType <*> bracedCode_ <*> genericSymlist
    <|> GrammarDecl <$> pDefaultPrec_
    <|> GrammarDecl <$> pNoDefaultPrec_
    <|> GrammarDecl <$> pCode_ <*> bracedCode_
    <|> GrammarDecl <$> pCode_ <*> identifier_ <*> bracedCode_
    <|> GrammarDecl <$> pUnion_ <*> optional identifier_ <*> bracedCode_

codePropsType :: Parser CodePropsType
codePropsType =
        CodePropsType <$> pDestructor_
    <|> CodePropsType <$> pPrinter_

symbolDeclaration :: Parser SymbolDecl
symbolDeclaration =
        pNterm_ <*> nTermDecls
    <|> pToken_ <*> tokenDecls
    <|> pType_ <*> symbolDecls
    <|> precedenceDeclarator <*> tokenDeclsForPrec

precedenceDeclarator :: Parser (GrammarNode Token)
precedenceDeclarator =
        pLeft_
    <|> pRight_
    <|> pNonAssoc_
    <|> pPrecedence_

optTag :: Parser (GrammarNode Token)
optTag = option [] tag_

genericSymlist :: Parser (GrammarNode Token)
genericSymlist = many genericSymlistItem

genericSymlistItem :: Parser (GrammarNode Token)
genericSymlistItem =
        symbol
    <|> tag

tag :: Parser (GrammarNode Token)
tag =
        tag_
    <|> tagAny_
    <|> tagNone_

nTermDecls :: Parser (GrammarNode Token)
nTermDecls = tokenDecls

tokenDecls :: Parser (GrammarNode Token)
tokenDecls = some $ tokenDecl <|> tag_ <*> tokenDecl

tokenDecl :: Parser (GrammarNode Token)
tokenDecl = id <*> optInt <*> optStringAsId

optInt :: Parser (GrammarNode Token)
optInt = option [] integer_

tokenDeclsForPrec :: Parser (GrammarNode Token)
tokenDeclsForPrec = many $
        some tokenDeclForPrec
    <|> tag_ <*> some tokenDeclForPrec

tokenDeclForPrec :: Parser (GrammarNode Token)
tokenDeclForPrec =
        id <*> optInt
    <|> stringAsId

symbolDecls :: Parser (GrammarNode Token)
symbolDecls = many $
        some symbolDecl
    <|> tag_ <*> some symbolDecl

symbolDecl :: Parser (GrammarNode Token)
symbolDecl = symbol

grammar :: Parser (GrammarNode Token)
grammar = some rulesOrGrammarDeclaration

rulesOrGrammarDeclaration :: Parser (GrammarNode Token)
rulesOrGrammarDeclaration =
        rules
    <|> grammarDeclaration <*> semicolon_

rules :: Parser (GrammarNode Token)
rules = idColon optNamedRef colon_ rhses

rhses :: Parser (GrammarNode Token)
rhses = some (rhs <*> pipe_ <|> rhs) <*> semicolon_

rhs :: Parser (GrammarNode Token)
rhs =
        rhs <*> symbol <*> optNamedRef
    <|> rhs <*> optTag <*> bracedCode_ <*> optNamedRef
    <|> rhs <*> predicate_
    <|> rhs <*> pEmpty_
    <|> rhs <*> pPrec_ <*> symbol
    <|> rhs <*> pDprec_ <*> integer_
    <|> rhs <*> pMerge_ <*> tag_
    <|> rhs <*> pExpect_ <*> integer_
    <|> rhs <*> pExpectRr_ <*> integer_

optNamedRef :: Parser (GrammarNode Token)
optNamedRef = option [] bracketedId_

value :: Parser (GrammarNode Token)
value =
        identifier_
    <|> string_
    <|> bracedCode_

id :: Parser (GrammarNode Token)
id =
        identifier_
    <|> character_

idColon :: Parser (GrammarNode Token)
idColon = idColon_

symbol :: Parser (GrammarNode Token)
symbol =
        id
    <|> stringAsId

stringAsId :: Parser (GrammarNode Token)
stringAsId = string_

optStringAsId :: Parser (GrammarNode Token)
optStringAsId = option [] stringAsId

optEpilogue :: Parser (Maybe Epilogue)
optEpilogue = optional $ Epilogue <$> percentPercent_ <*> epilogue_)
