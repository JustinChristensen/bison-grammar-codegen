module Bison.Grammar.Parser where

import Bison.Grammar.Types
import Data.Text (Text)
import Data.Attoparsec.Text hiding (parse)

parseName :: Parser Text
parseName = takeWhile1 (not . (== ':')) <* char ':'

parseProductions :: Parser [Production]
parseProductions = do
    skipSpace
    productions <- sepBy1' parseProduction skipSpace
    skipSpace
    endOfInput
    pure productions

parseProduction :: Parser Production
parseProduction = Production <$> parseName <*> parseClauses

parseClauses :: Parser [Clause]
parseClauses = sepBy1' parseClause parseAlt
    where
        parseAlt = skipSpace >> char '|' >> skipSpace

parseClause :: Parser Clause
parseClause = Clause <$> parseTokens

parseTokens :: Parser [Token]
parseTokens = many1' parseToken

parseToken :: Parser Token
parseToken = undefined

parse :: Text -> [Production]
parse = A.parseOnly parseProductions
