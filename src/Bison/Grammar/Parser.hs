{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Parser where

import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (Token)
import Bison.Grammar.Types
import Bison.Grammar.Lexer

grammarFile :: Parser GrammarFile
grammarFile = GrammarFile
    <$> many prologueDecl
    <*> (percentPercent_ *> many grammarRuleOrDecl)
    <*> optional epilogue

prologueDecl :: Parser PrologueDecl
prologueDecl =
        GrammarDeclPD <$> grammarDecl
    <|> ProloguePD <$> prologue_
    <|> FlagPD <$> pFlag_
    <|> DefinePD <$> (pDefine_ *> identifier_) <*> optional value
    <|> DefinesPD <$> (pDefines_ *> optional string_)
    <|> ErrorVerbosePD <* pErrorVerbose_
    <|> ExpectPD <$> (pExpect_ *> integer_)
    <|> ExpectRrPD <$> (pExpectRr_ *> integer_)
    <|> FilePrefixPD <$> (pFilePrefix_ *> string_)
    <|> GlrParserPD <$ pGlrParser_
    <|> InitialActionPD <$> (pInitialAction_ *> bracedCode_)
    <|> LanguagePD <$> (pLanguage_ *> string_)
    <|> NamePrefixPD <$> (pNamePrefix_ *> string_)
    <|> NoLinesPD <$ pNoLines_
    <|> NonDeterministicParserPD <$ pNonDeterministicParser_
    <|> OutputPD <$> (pOutput_ *> string_)
    <|> ParamPD <$> (pParam_ *> many bracedCode_)
    <|> PureParserPD <$ pPureParser_
    <|> RequirePD <$> (pRequire_ *> string_)
    <|> SkeletonPD <$> (pSkeleton_ *> string_)
    <|> TokenTablePD <$ pTokenTable_
    <|> VerbosePD <$ pVerbose_
    <|> YaccPD <$ pYacc_

grammarDecl :: Parser GrammarDecl
grammarDecl =
        SymbolDeclGD <$> symbolDecl
    <|> StartGD <$> (pStart_ *> symbol)
    <|> DestructorGD <$> (pDestructor_ *> bracedCode_) <*> some genericSymlistItem
    <|> PrinterGD <$> (pPrinter_ *> bracedCode_) <*> some genericSymlistItem
    <|> DefaultPrecGD <$> pDefaultPrec_
    <|> NoDefaultPrecGD <$> pNoDefaultPrec_
    <|> CodeGD <$> (pCode_ *> optional identifier_) <*> bracedCode_
    <|> UnionGD <$> (pUnion_ *> optional identifier_) <*> bracedCode_

symbolDecl:: Parser SymbolDecl
symbolDecl =
        NTermSD <$> (pNterm_ *> optional tag_) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TokenSD <$> (pToken_ *> optional tag_) <*> some tokenDecl <*> many (taggedDecls tokenDecl)
    <|> TypeSD <$> (pType_ *> optional tag_) <*> some symbol <*> many (taggedDecls symbol)
    <|> NTermSD <$> precedenceDecl <*> optional tag_ <*> some precTokenDecl <*> many (taggedDecls precTokenDecl)

taggedDecls :: Parser a -> Parser (TaggedDecls a)
taggedDecls p = TaggedDecls <$> tag_ <*> some p

tokenDecl :: Parser TokenDecl
tokenDecl = TokenDecl <$> id' <*> optional integer_ <*> optional string_

precTokenDecl :: Parser PrecTokenDecl
precTokenDecl =
        IdP <$> id' <*> optional integer_
    <|> StrP <$> string_

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
        Tag <$> tag_
    <|> TagAny <$ tagAny_
    <|> TagNone <$ tagNone_

grammarRuleOrDecl :: Parser GrammarRuleOrDecl
grammarRuleOrDecl =
        RuleGD <$> rule
    <|> GrammarDeclGD <$> (grammarDecl <* semicolon_)

rule :: Parser Rule
rule = Rule
    <$> idColon_
    <*> optional bracketedId_
    <*> (colon_ *> rhses `sepBy1` pipe_ <* semicolon_)

rhses :: Parser Rhses
rhses = Rhses <$> some rhs

rhs :: Parser Rhs
rhs =
        SymR <$> symbol <*> optional bracketedId_
    <|> TagR <$> optional tag_ <*> bracedCode_ <*> optional bracketedId_
    <|> PredR <$> predicate_
    <|> EmptyR <$ pEmpty_
    <|> PrecR <$> (pPrec_ *> symbol)
    <|> DprecR <$> (pDprec_ *> integer_)
    <|> MergeR <$> (pMerge_ *> tag_)
    <|> ExpectR <$> (pExpect_ *> integer_)
    <|> ExpectRrR <$> (pExpectRr_ *> integer_)

value :: Parser Value
value =
        IdV <$> identifier_
    <|> StrV <$> string_
    <|> CodeV <$> bracedCode_

id' :: Parser Id
id' =
        Id <$> identifier_
    <|> Char <$> character_

symbol :: Parser Symbol
symbol =
        IdS <$> id'
    <|> StrS <$> string_

epilogue :: Parser Epilogue
epilogue = Epilogue <$> (percentPercent_ *> epilogue_)

