module Bison.Grammar.Codegen (
    productions
) where

import Text.Megaparsec hiding (Token)
import qualified Data.Text.IO as T (getContents)
import Bison.Grammar.Parser (grammarFile)
import Bison.Grammar.Utils
import Bison.Grammar.Types

productions :: ([Token] -> IO ()) -> IO ()
productions _ = do
    grammarSpec <- T.getContents
    let grammar = parse grammarFile "stdin" grammarSpec
    case grammar of
        Right t -> prettyPrint t
        Left e -> putStr (errorBundlePretty e)

