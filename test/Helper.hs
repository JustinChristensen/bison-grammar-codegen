module Helper where

import Data.Void
import Data.Text
import Bison.Grammar.Types
import qualified Text.Megaparsec as M

parser' :: ParseState -> Parser a -> (Text -> Either (M.ParseErrorBundle Text Void) a)
parser' state p = M.parse (runParser state p) ""

parser :: Parser a -> (Text -> Either (M.ParseErrorBundle Text Void) a)
parser = parser' (ParseState Prologue)

