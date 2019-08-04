module Bison.Grammar.Codegen (
    productions
) where

import Text.Megaparsec hiding (Token)
import qualified Data.Text.IO as T (getContents)
import Bison.Grammar.Lexer
import Bison.Grammar.Types

productions :: ([Token] -> IO ()) -> IO ()
productions _ = do
    grammarSpec <- T.getContents
    let mTokens = scan grammarSpec
    case mTokens of
        Right t -> mapM_ (putStrLn . show) t
        Left e -> putStr (errorBundlePretty e)

