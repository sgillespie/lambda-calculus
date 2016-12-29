module Main where

import Data.Version

import Options.Applicative hiding (ParseError)
import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Readline (readlineBackend)

import qualified Paths_lambda_calculator as P

import Language.Lambda
import Language.Lambda.Util.PrettyPrint
import Language.SystemF

main :: IO ()
main = execParser opts >>= runShell'
  where opts = info (helper <*> cliParser)
                    (briefDesc <> progDesc "A Lambda Calculus Interpreter")

-- Option Parsing
data CliOptions = CliOptions {
  language :: Eval Language,
  version :: Bool
  }

-- Supported Languages:
-- 
--  * Untyped Lambda Calculus
--  * System F
data Language 
  = Untyped
  | SystemF

-- The result of an evaluation
type Result a = Either a -- An error
                       a -- The result

-- Represent a language together with its evaluation function
data Eval a = Eval a (String -> Result String)

untyped :: Eval Language
untyped = Eval Untyped eval
  where eval = fromEvalString Language.Lambda.evalString

systemf :: Eval Language
systemf = Eval SystemF eval
  where eval = fromEvalString Language.SystemF.evalString

-- Take a typed evaluation function and return a function that returns a result
-- 
-- For example:
--   (String -> Either ParseError (LambdaExpr String)) -> (String -> Result String)
--   (String -> Either ParseError (SystemFExpr String String)) -> (String -> Result String)
fromEvalString :: (Show s, PrettyPrint p)
               => (String -> Either s p)
               -> (String -> Result String)
fromEvalString f = either (Left . show) (Right . prettyPrint) . f

cliParser :: Parser CliOptions
cliParser = CliOptions 
  <$> flag untyped systemf (long "system-f" <> 
                            short 'f' <> 
                            help "Use the System F interpreter")

  <*> switch (long "version" <> 
              short 'v' <> 
              help "Print the version")

-- Interactive Shell
runShell' :: CliOptions -> IO ()
runShell' CliOptions{version=True} = putStrLn version'
runShell' CliOptions{language=Eval lang eval} 
  = runShell (mkShellDesc lang eval) readlineBackend ()

mkShellDesc :: Language 
            -> (String -> Result String)
            -> ShellDescription ()
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

eval :: (String -> Result String) -> String -> Sh s' ()
eval f = either shellPutErrLn shellPutStrLn . f

-- Get the current version
version' :: String
version' = showVersion P.version
 
