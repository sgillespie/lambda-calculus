module Main where

import Data.Version

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

-- | The result of an evaluation
type Result a state = Either a           -- ^ An error
                             (a, state)  -- ^ The result along with its state

-- | Represent a language together with its evaluation function
data Eval a = Eval a (String -> Result String (Globals String String))

untyped :: Eval Language 
untyped = Eval Untyped eval
  where eval = fromEvalString Lambda.evalString

systemf :: Eval Language
systemf = Eval SystemF eval
  where eval = fromEvalString SystemF.evalString

-- | Take a typed evaluation function and return a function that returns a result
-- 
--   For example:
--     (String -> Either ParseError (LambdaExpr String)) -> (String -> Result String)
--     (String -> Either ParseError (SystemFExpr String String)) -> (String -> Result String)
fromEvalString :: (Show s, PrettyPrint p)
               => (String -> Either s p)
               -> (String -> Result String (Globals String String))
fromEvalString f = either (Left . show) (Right . toResult) . f
  -- TODO[sgillespie]: Remove placeholder below
  where toResult expr = (prettyPrint expr, GlobalsUntyped Map.empty)

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
  = runShell (mkShellDesc lang eval) haskelineBackend ()

mkShellDesc :: Language 
            -> (String -> Result String (Globals String String))
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

eval :: (String -> Result String (Globals String String)) -> String -> Sh s' ()
eval f = either shellPutErrLn shellPutStrLn . fmap fst . f

-- | Get the current version
version' :: String
version' = showVersion P.version
 
