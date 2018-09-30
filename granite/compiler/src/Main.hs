module Main
  ( main
  ) where

import Control.Applicative ((<**>), many)
import Control.Monad (join)
import Control.Monad.Trans.Except (runExcept)
import Data.Foldable (fold)

import qualified Data.Text.IO as Text
import qualified LLVM.IRBuilder as IRB
import qualified Options.Applicative as Opt
import qualified Text.Parsec as Parser

import Granite.Organizational.Abstract (Definition)
import Granite.Organizational.Interface (collectInterface)
import Granite.Organizational.TypeCheck (typeCheckImplementation)

import qualified Granite.Behavioral.Llvm as Llvm
import qualified Granite.Organizational.Llvm as Llvm
import qualified Granite.Organizational.Parse as Parse

data Config =
  Config
    { configInterfaceFiles      :: [FilePath]
    , configImplementationFiles :: [FilePath] }
  deriving stock (Eq, Show)

configParser :: Opt.Parser Config
configParser = do
  interfaceFiles <- many $
    Opt.option Opt.str $
      fold [ Opt.short 'i'
           , Opt.metavar "FILE ..."
           , Opt.help "Read interfaces from these files" ]

  implementationFiles <- many $
    Opt.argument Opt.str $
      fold [ Opt.metavar "FILE ..."
           , Opt.help "Read implementations from these files" ]

  pure $ Config interfaceFiles implementationFiles

main :: IO ()
main = do
  config <- Opt.execParser $
    Opt.info (configParser <**> Opt.helper) $
      fold [ Opt.fullDesc ]

  interfaceAST      <- join <$> traverse parse (configInterfaceFiles config)
  implementationAST <- join <$> traverse parse (configImplementationFiles config)

  interface <- either (fail . show) pure $
                 collectInterface interfaceAST

  () <- either (fail . show) pure $
          typeCheckImplementation interface implementationAST

  ((), llvmDefinitions) <- either (fail . show) pure $
    runExcept . IRB.runModuleBuilderT IRB.emptyModuleBuilder $ do
      rts <- Llvm.buildRts
      globals <- Llvm.buildInterface interfaceAST
      Llvm.buildImplementation rts globals implementationAST

  print llvmDefinitions

  pure ()

parse :: FilePath -> IO [Definition]
parse file = do
  let parser = many Parse.definition <* Parser.eof
  text <- Text.readFile file
  either (fail . show) pure $
    Parser.parse parser file text
