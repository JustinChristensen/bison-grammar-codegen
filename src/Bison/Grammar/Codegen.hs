module Bison.Grammar.Codegen (
    productions
) where

import Bison.Grammar.Types

productions :: ([Production] -> IO ()) -> IO ()
productions f = do
    grammar <- getContents
    print grammar
    f []

