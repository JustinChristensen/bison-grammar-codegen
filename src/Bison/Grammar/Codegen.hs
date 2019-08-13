module Bison.Grammar.Codegen (
    withFile
,   withFileText
,   productions
,   clauses
,   symbols
,   render
,   prettyPrint
) where

import qualified Data.Text.IO as T (getContents, readFile)
import Data.Text (Text)
import Control.Monad.Reader
import Text.Megaparsec
import Bison.Grammar.Parser (grammarFile)
import Bison.Grammar.Utils (Pretty)
import qualified Bison.Grammar.Gram as G (toProductions)
import qualified Bison.Grammar.Utils as U
import Bison.Grammar.Types

withFile :: Maybe FilePath -> Codegen () -> IO ()
withFile mfp rdr = do
        input <- maybe T.getContents T.readFile mfp
        withFileText input dfp rdr
    where dfp = maybe "stdin" id mfp

withFileText :: Text -> FilePath -> Codegen () -> IO ()
withFileText input dfp rdr =
    case parse grammarFile dfp input of
        Left eb -> putStrLn $ errorBundlePretty eb
        Right ast -> runReaderT rdr $ CodegenContext ast

productions :: Codegen [Production]
productions = G.toProductions [] <$> asks grammarAST

render :: Codegen ()
render = undefined

prettyPrint :: Pretty a => a -> Codegen ()
prettyPrint = liftIO . U.prettyPrint


