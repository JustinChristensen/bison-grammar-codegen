module Main where

import Bison.Grammar.Codegen (productions)
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

-- runCodegen :: IO ()
-- runCodegen = do
--     productions $ \_ -> pure ()

main :: IO ()
main = do
    args <- readArgs
    if printVersion args then runVersion
    else runCodegen

