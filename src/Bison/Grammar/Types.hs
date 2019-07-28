module Bison.Grammar.Types where

import Data.Text (Text)

data Production = Production {
        name :: Text
    ,   clauses :: [Clause]
    } deriving (Show, Read, Eq, Ord)

newtype Clause = Clause {
        tokens :: [Token]
    } deriving (Show, Read, Eq, Ord)

data Token
    = Terminal Text
    | NonTerminal Text
    | Directive Text
    deriving (Show, Read, Eq, Ord)
