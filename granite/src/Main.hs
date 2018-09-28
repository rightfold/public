module Main
  ( main
  ) where

import Control.Applicative (many)

import qualified Data.Text.IO as Text
import qualified Text.Parsec as Parser

import qualified Granite.Organizational.Parse as Parse

main :: IO ()
main = do
  let sourceFile = "<stdin>"
  sourceText <- Text.getContents

  let parser = many Parse.definition <* Parser.eof
  print $ Parser.parse parser sourceFile sourceText
