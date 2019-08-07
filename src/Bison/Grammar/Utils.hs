{-# LANGUAGE OverloadedStrings #-}
module Bison.Grammar.Utils (
  pretty
, prettyPrint
) where

import Prelude hiding (unwords, unlines, putStrLn)
import Data.Text
import Data.Text.IO
import qualified Data.List as L (intersperse)
import Bison.Grammar.Types

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

class Pretty a where
    pretty :: a -> Text

instance Pretty a => Pretty [a] where
    pretty xs = unlines $ pretty <$> xs

instance Pretty a => Pretty (Maybe a) where
    pretty mx = maybe mempty pretty mx

instance Pretty GrammarFile where
    pretty (GrammarFile pds grs mep) =
           pretty pds
        <> unlines ["%%", pretty grs]
        <> pretty mep

instance Pretty PrologueDecl where
    pretty (GrammarDeclPD gd) = pretty gd
    pretty (ProloguePD p) = pretty p
    pretty (FlagPD pf) = pretty pf
    pretty (DefinePD idt mv) = unwords ["%define", pretty idt, pretty mv]
    pretty (DefinesPD ms) = unwords ["%defines", pretty ms]
    pretty (ErrorVerbosePD) = "%error-verbose"
    pretty (ExpectPD int) = unwords ["%expect", pretty int]
    pretty (ExpectRrPD int) = unwords ["%expect-rr", pretty int]
    pretty (FilePrefixPD str) = unwords ["%file-prefix", pretty str]
    pretty (GlrParserPD) = "%glr-parser"
    pretty (InitialActionPD bc) = unwords ["%initial-action", pretty bc]
    pretty (LanguagePD str) = unwords ["%language", pretty str]
    pretty (NamePrefixPD str) = unwords ["%name-prefix", pretty str]
    pretty (NoLinesPD) = "%no-lines"
    pretty (NonDeterministicParserPD) = "%nondeterministic-parser"
    pretty (OutputPD str) = unwords ["%output", pretty str]
    pretty (ParamPD pp bc) = unwords [pretty pp, pretty bc]
    pretty (PureParserPD) = "%pure-parser"
    pretty (RequirePD str) = unwords ["%require", pretty str]
    pretty (SkeletonPD str) = unwords ["%skeleton", pretty str]
    pretty (TokenTablePD) = "%token-table"
    pretty (VerbosePD) = "%verbose"
    pretty (YaccPD) = "%yacc"

instance Pretty GrammarDecl where
    pretty (SymbolDeclGD sd) = pretty sd
    pretty (StartGD s) = unwords ["%start", pretty s]
    pretty (DestructorGD bc gsl) = unwords ["%destructor", pretty bc, unwords $ pretty <$> gsl]
    pretty (PrinterGD bc gsl) = unwords ["%printer", pretty bc, unwords $ pretty <$> gsl]
    pretty (DefaultPrecGD) = "%default-prec"
    pretty (NoDefaultPrecGD) = "%no-default-prec"
    pretty (CodeGD midt bc) = unwords ["%code", pretty midt, pretty bc]
    pretty (UnionGD midt bc) = unwords ["%union", pretty midt, pretty bc]

instance Pretty SymbolDecl where
    pretty x = case x of
        (NTermSD mt ds tds) -> p "%nterm" mt ds tds
        (TokenSD mt ds tds) -> p "%token" mt ds tds
        (TypeSD mt ds tds) -> p "%type" mt ds tds
        (PrecedenceDeclSD pd mt ds tds) -> p (pretty pd) mt ds tds
        where
            p d mt ds tds =
                unwords [d, pretty mt, unwords (pretty <$> ds)] <>
                pretty tds

instance Pretty d => Pretty (TaggedDecls d) where
    pretty (TaggedDecls t ds) = unlines [pretty t, pretty ds]

instance Pretty TokenDecl where
    pretty (TokenDecl i mint mstr) = unwords [pretty i, pretty mint, pretty mstr]

instance Pretty PrecTokenDecl where
    pretty (IdP i mint) = unwords [pretty i, pretty mint]
    pretty (StrP str) = pretty str

instance Pretty PrecedenceDecl where
    pretty (LeftPD) = "%left"
    pretty (RightPD) = "%right"
    pretty (NonAssocPD) = "%nonassoc"
    pretty (PrecedencePD) = "%precedence"

instance Pretty GenericSymlistItem where
    pretty (SymbolGS sym) = pretty sym
    pretty (TagGS t) = pretty t

instance Pretty Tag where
    pretty (Tag t) = pretty t
    pretty (TagAny) = "<*>"
    pretty (TagNone) = "<>"

instance Pretty GrammarRuleOrDecl where
    pretty (RuleGD r) = pretty r
    pretty (GrammarDeclGD gd) = pretty gd

instance Pretty Rule where
    pretty (Rule idc mbid rhses) = unlines [
          pretty idc <> pretty mbid <> ":"
        , mconcat $ L.intersperse " |\n " (pretty <$> rhses)
        ]

instance Pretty Rhses where
    pretty (Rhses rs) = unwords (pretty <$> rs)

instance Pretty Rhs where
    pretty (SymR s mbid) = unwords [pretty s, pretty mbid]
    pretty (TagR mt bc mbid) = unwords [pretty mt, pretty bc, pretty mbid]
    pretty (PredR bp) = pretty bp
    pretty (EmptyR) = "%empty"
    pretty (PrecR s) = unwords ["%prec", pretty s]
    pretty (DprecR i) = unwords ["%dprec", pretty i]
    pretty (MergeR t) = unwords ["%merge", pretty t]
    pretty (ExpectRrR i) = unwords ["%expect-rr", pretty i]
    pretty (ExpectR i) = unwords ["%expect", pretty i]

instance Pretty Value where
    pretty (IdV i) = pretty i
    pretty (StrV s) = pretty s
    pretty (CodeV bc) = pretty bc

instance Pretty Id where
    pretty (Id i) = pretty i
    pretty (Char c) = pretty c

instance Pretty Symbol where
    pretty (IdS i) = pretty i
    pretty (StrS s) = pretty s

instance Pretty Epilogue where
    pretty (Epilogue e) = pretty e

instance Pretty StringT where
    pretty (StringT t) = "\"" <> t <> "\""

instance Pretty BracedCodeT where
    pretty (BracedCodeT t) = t

instance Pretty BracedPredicateT where
    pretty (BracedPredicateT t) = t

instance Pretty BracketedIdT where
    pretty (BracketedIdT t) = t

instance Pretty CharT where
    pretty (CharT c) = "'" <> singleton c <> "'"

instance Pretty EpilogueT where
    pretty (EpilogueT t) = unlines ["%%", t]

instance Pretty IdT where
    pretty (IdT t) = t

instance Pretty IdColonT where
    pretty (IdColonT t) = t

instance Pretty IntT where
    pretty (IntT i) = pack $ show i

instance Pretty PrologueT where
    pretty (PrologueT t) = t

instance Pretty PercentFlagT where
    pretty (PercentFlagT t) = t

instance Pretty PercentParamT where
    pretty (PercentParamT t) = t

instance Pretty TagT where
    pretty (TagT t) = t

