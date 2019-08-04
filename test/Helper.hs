{-# LANGUAGE OverloadedStrings #-}
module Helper where

import Control.Monad.State (runStateT)
import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Void
import Data.Text (Text)
import Bison.Grammar.Types
import qualified Text.Megaparsec as M

parser' :: ParseState -> Parser a -> (Text -> Either (M.ParseErrorBundle Text Void) a)
parser' state p = evalParser state p ""

parser :: Parser a -> (Text -> Either (M.ParseErrorBundle Text Void) a)
parser = parser' (ParseState Prologue)

isAtomicGiven :: Show a => Parser a -> Text -> Expectation
isAtomicGiven p v = M.runParser' (runStateT p initParserState) (initialState v)
    `failsLeaving` v

initParserState :: ParseState
initParserState = ParseState Prologue

isPrologue :: Token -> Bool
isPrologue x = case x of
    PROLOGUE _ -> True
    _ -> False

isBracedCode :: Token -> Bool
isBracedCode x = case x of
    BRACED_CODE _ -> True
    _ -> False

isBracedPredicate :: Token -> Bool
isBracedPredicate x = case x of
    BRACED_PREDICATE _ -> True
    _ -> False

