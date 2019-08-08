module Bison.Grammar.AST where

import Data.Maybe (catMaybes)
import Bison.Grammar.Types
import Data.Text (singleton)
import qualified Data.Set as S
import Data.Set (member)

class ToProductions a where
    toProductions :: [Production] -> a -> [Production]

instance ToProductions a => ToProductions [a] where
    toProductions ps = concatMap (toProductions ps)

instance ToProductions GrammarFile where
    toProductions ps (GrammarFile _ grs _) = labelNTs $ foldl toProductions ps grs

instance ToProductions GrammarRuleOrDecl where
    toProductions ps (RuleGD r) = toProductions ps r
    toProductions ps _ = ps

instance ToProductions Rule where
    toProductions ps (Rule (IdColonT n) _ rhses) =
            let prod = Production n (or $ emptyRhses <$> rhses) (toClause <$> rhses)
            in mergeOrAppend prod ps
        where
            emptyRhses (Rhses rhs) = or $ emptyRhs <$> rhs
            emptyRhs r = case r of
                EmptyR -> True
                _ -> False

toClause :: Rhses -> Clause
toClause (Rhses rhses) = Clause (catMaybes $ toSymbol <$> rhses)

toSymbol :: Rhs -> Maybe Symbol
toSymbol (SymR sym _) = Just $ Terminal $ case sym of
    IdS (Id (IdT s)) -> s
    IdS (Char (CharT c)) -> singleton c
    StrS (StringT s) -> s
toSymbol _ = Nothing

mergeOrAppend :: Production -> [Production] -> [Production]
mergeOrAppend cand ps =
        let (start, end) = break (nameEq cand) ps
        in case end of
            [] -> start ++ [cand]
            (exis:rest) -> start ++ ((exis <> cand) : rest)

labelNTs :: [Production] -> [Production]
labelNTs ps = labelProd <$> ps
    where
        names = S.fromList (name <$> ps)
        toNT (Terminal s) | s `member` names = NonTerminal s
                          | otherwise = Terminal s
        toNT s = s
        labelProd (Production n e cs) = Production n e (labelClause <$> cs)
        labelClause (Clause syms) = Clause $ toNT <$> syms

