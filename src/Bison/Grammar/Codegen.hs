module Bison.Grammar.Codegen (
    productions
) where

import qualified Data.Text.IO as T (getContents)
import Bison.Grammar.Parser
import Bison.Grammar.Types

productions :: ([Production] -> IO ()) -> IO ()
productions f = do
    grammar <- T.getContents
    f $ parse grammar

