{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Parser where

import Prelude hiding (id)
import Control.Applicative hiding (many, some)
import Bison.Grammar.Types
import Bison.Grammar.Lexer

input :: Parser [Token]
input = prologueDeclarations #<> percentPercent_ #<> grammar #<> optEpilogue

prologueDeclarations :: Parser [Token]
prologueDeclarations = mconcat <$> many prologueDeclaration

prologueDeclaration :: Parser [Token]
prologueDeclaration =
        grammarDeclaration
    <|> prologue_
    <|> pFlag_
    <|> pDefine_ #<> variable #<> value
    <|> pDefines_
    <|> pDefines_ #<> string_
    <|> pErrorVerbose_
    <|> pExpect_ #<> integer_
    <|> pExpectRr_ #<> integer_
    <|> pFilePrefix_ #<> string_
    <|> pGlrParser_
    <|> pInitialAction_
    <|> pLanguage_ #<> string_
    <|> pNamePrefix_ #<> string_
    <|> pNoLines_
    <|> pNonDeterministicParser_
    <|> pOutput_ #<> string_
    <|> pParam_ #<> params
    <|> pPureParser_
    <|> pRequire_ #<> string_
    <|> pSkeleton_ #<> string_
    <|> pTokenTable_
    <|> pVerbose_
    <|> pYacc_

params :: Parser [Token]
params = many bracedCode_

grammarDeclaration :: Parser [Token]
grammarDeclaration =
        symbolDeclaration
    <|> pStart_ #<> symbol
    <|> codePropsType #<> bracedCode_ #<> genericSymlist
    <|> pDefaultPrec_
    <|> pNoDefaultPrec_
    <|> pCode_ #<> bracedCode_
    <|> pCode_ #<> identifier_ #<> bracedCode_
    <|> pUnion_ #<> unionName #<> bracedCode_

codePropsType :: Parser [Token]
codePropsType =
        pDestructor_
    <|> pPrinter_

unionName :: Parser [Token]
unionName = option [] identifier_

symbolDeclaration :: Parser [Token]
symbolDeclaration =
        pNterm_ #<> nTermDecls
    <|> pToken_ #<> tokenDecls
    <|> pType_ #<> symbolDecls
    <|> precedenceDeclarator #<> tokenDeclsForPrec

precedenceDeclarator :: Parser [Token]
precedenceDeclarator =
        pLeft_
    <|> pRight_
    <|> pNonAssoc_
    <|> pPrecedence_

optTag :: Parser [Token]
optTag = option [] tag_

genericSymlist :: Parser [Token]
genericSymlist = many genericSymlistItem

genericSymlistItem :: Parser [Token]
genericSymlistItem =
        symbol
    <|> tag

tag :: Parser [Token]
tag =
        tag_
    <|> tagAny_
    <|> tagNone_

nTermDecls :: Parser [Token]
nTermDecls = tokenDecls

tokenDecls :: Parser [Token]
tokenDecls = some $ tokenDecl <|> tag_ #<> tokenDecl

tokenDecl :: Parser [Token]
tokenDecl = id #<> optInt #<> optStringAsId

optInt :: Parser [Token]
optInt = option [] integer_

tokenDeclsForPrec :: Parser [Token]
tokenDeclsForPrec = many $
        some tokenDeclForPrec
    <|> tag_ #<> some tokenDeclForPrec

tokenDeclForPrec :: Parser [Token]
tokenDeclForPrec =
        id #<> optInt
    <|> stringAsId

symbolDecls :: Parser [Token]
symbolDecls = many $
        some symbolDecl
    <|> tag_ #<> some symbolDecl

symbolDecl :: Parser [Token]
symbolDecl = symbol

grammar :: Parser [Token]
grammar = some rulesOrGrammarDeclaration

rulesOrGrammarDeclaration :: Parser [Token]
rulesOrGrammarDeclaration =
        rules
    <|> grammarDeclaration #<> semicolon_

rules :: Parser [Token]
rules = idColon optNamedRef colon_ rhses

rhses :: Parser [Token]
rhses = some (rhs #<> pipe_ <|> rhs) #<> semicolon_

rhs :: Parser [Token]
rhs =
        rhs #<> symbol #<> optNamedRef
    <|> rhs #<> optTag #<> bracedCode_ #<> optNamedRef
    <|> rhs #<> predicate_
    <|> rhs #<> pEmpty_
    <|> rhs #<> pPrec_ #<> symbol
    <|> rhs #<> pDprec_ #<> integer_
    <|> rhs #<> pMerge_ #<> tag_
    <|> rhs #<> pExpect_ #<> integer_
    <|> rhs #<> pExpectRr_ #<> integer_

optNamedRef :: Parser [Token]
optNamedRef = option [] bracketedId_

variable :: Parser [Token]
variable = identifier_

value :: Parser [Token]
value =
        identifier_
    <|> string_
    <|> bracedCode_

id :: Parser [Token]
id =
        identifier_
    <|> character_

idColon :: Parser [Token]
idColon = idColon_

symbol :: Parser [Token]
symbol =
        id
    <|> stringAsId

stringAsId :: Parser [Token]
stringAsId = string_

optStringAsId :: Parser [Token]
optStringAsId = option [] stringAsId

optEpilogue :: Parser [Token]
optEpilogue = option [] (percentPercent_ #<> epilogue_)
