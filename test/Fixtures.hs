module Fixtures where

import Prelude hiding (readFile)
import Data.Text
import Data.Text.IO (readFile)

prologueFixture :: IO Text
prologueFixture = readFile "./test/Fixtures/prologue.y"

bracedCodeFixture :: IO Text
bracedCodeFixture = readFile "./test/Fixtures/bracedCode.y"
