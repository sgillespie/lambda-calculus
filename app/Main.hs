module Main where

import Data.Version

import Control.Monad
import Data.Bifunctor
import Data.Semigroup
import Options.Applicative hiding (ParseError)
import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Haskeline (haskelineBackend)

import qualified Data.Map as Map
import qualified Paths_lambda_calculator as P

import Language.Lambda.Util.PrettyPrint

import qualified Language.Lambda as Lambda
import qualified Language.SystemF as SystemF

main :: IO ()
main = execParser opts >>= runShell'
  where opts = info (helper <*> cliParser)
                    (briefDesc <> progDesc "A Lambda Calculus Interpreter")

-- | Option Parsing
data CliOptions = CliOptions {
  language :: Eval Language,
  version :: Bool
}

-- | Supported Languages:
-- 
--    * Untyped Lambda Calculus
--    * System F
data Language 
  = Untyped
  | SystemF

-- | Globals are Maps that map names to expressions.
data Globals n t
  = GlobalsUntyped (Map.Map n (Lambda.LambdaExpr n))
  | GlobalsSystemF (Map.Map n (SystemF.SystemFExpr n t))

newGlobals :: Language -> Globals n t
newGlobals Untyped = GlobalsUntyped Map.empty
newGlobals SystemF = GlobalsSystemF Map.empty

-- | The result of an evaluation
type Result a state = Either a           -- ^ An error
                             (a, state)  -- ^ The result along with its state

-- | Represent a language together with its evaluation function
data Eval a = Eval a (Globals String String -> String -> Result String (Globals String String))

untyped :: Eval Language 
untyped = Eval Untyped eval
  where eval (GlobalsUntyped g) s = bimap show toResult $ Lambda.evalString g s
        toResult (e, g) = (prettyPrint e, GlobalsUntyped g)

systemf :: Eval Language
systemf = Eval SystemF eval
  where eval (GlobalsSystemF g) s = bimap show toResult $ SystemF.evalString g s
        toResult (e, g) = (prettyPrint e, GlobalsSystemF g)

cliParser :: Parser CliOptions
cliParser = CliOptions 
  <$> flag untyped systemf (long "system-f" <> 
                            short 'f' <> 
                            internal <>    -- this is a secret feature
                            help "Use the System F interpreter")

  <*> switch (long "version" <> 
              short 'v' <> 
              help "Print the version")

-- | Interactive Shell
runShell' :: CliOptions -> IO ()
runShell' CliOptions{version=True} = putStrLn version'
runShell' CliOptions{language=Eval lang eval} 
  = void $ runShell (mkShellDesc lang eval) haskelineBackend (newGlobals lang)

mkShellDesc :: Language 
            -> (Globals String String -> String -> Result String (Globals String String))
            -> ShellDescription (Globals String String)
mkShellDesc language f = shellDesc' $ mkShellDescription commands (eval f)
  where shellDesc' d = d {
          greetingText = Just shellGreeting,
          prompt = shellPrompt language
          }

shellGreeting :: String
shellGreeting = "Lambda Calculator (" ++ version' ++ ")\nType :h for help\n"
  
shellPrompt :: Language -> s -> IO String
shellPrompt language _ = return $ prefix language : " > "
  where prefix Untyped = lambda
        prefix SystemF = upperLambda

commands :: [ShellCommand s]
commands = [
  exitCommand "q",
  helpCommand "h"
  ]

eval :: (Globals String String -> String -> Result String (Globals String String))
     -> String
     -> Sh (Globals String String) ()
eval f str = do
  globals <- getShellSt

  case f globals str of
    Left err -> shellPutErrLn err
    Right (result, globals') -> do
      putShellSt globals'
      shellPutStr result

-- | Get the current version
version' :: String
version' = showVersion P.version
