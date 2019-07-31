module Bison.Grammar.Codegen (
    productions
) where

import qualified Data.Text.IO as T (getContents)
import Bison.Grammar.Lexer
import Bison.Grammar.Types

productions :: ([Token] -> IO ()) -> IO ()
productions f = do
    grammarSpec <- T.getContents
    let mTokens = scan grammarSpec
    case mTokens of
        Just t -> print t
        _ -> pure ()

