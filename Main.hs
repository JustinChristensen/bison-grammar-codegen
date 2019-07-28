module Main where

import Bison.Grammar.Codegen (productions)
import Options.Applicative

data Options = Options

optionParser :: Parser Options
optionParser = pure Options

readArgs :: IO Options
readArgs = execParser pInfo
    where
        pInfo = info (optionParser <**> helper) fullDesc

main :: IO ()
main = do
    args <- readArgs
    productions $ \_ -> pure ()
