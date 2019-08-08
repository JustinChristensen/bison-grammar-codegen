module Main where

import Bison.Grammar.Types (grammarAst)
import Bison.Grammar.Codegen (withFile, prettyPrint)
import Control.Monad.Reader
import Options.Applicative
import Data.Version (showVersion)
import Paths_bison_grammar_codegen (version)

data Args = Args {
        printVersion :: Bool
    } deriving (Show, Read, Eq)

argParser :: Parser Args
argParser = Args
    <$> switch (short 'v' <> help "print version information and exit")

readArgs :: IO Args
readArgs = execParser pInfo
    where
        pInfo = info (argParser <**> helper) fullDesc

runVersion :: IO ()
runVersion = putStrLn (showVersion version)

runCodegen :: IO ()
runCodegen = do
    withFile Nothing $ do
        ast <- asks grammarAst
        prettyPrint ast

main :: IO ()
main = do
    args <- readArgs
    if printVersion args then runVersion
    else runCodegen

