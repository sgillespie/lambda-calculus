module Main where

import Data.Version

import Options.Applicative
import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Readline (readlineBackend)

import Language.Lambda
import qualified Paths_lambda_calculator as P

main :: IO ()
main = execParser opts >>= runShell'
  where opts = info (helper <*> cliParser)
                    (briefDesc <> progDesc "A Lambda Calculus Interpreter")

-- Option Parsing
data CliOptions = CliOptions {
  language :: Language,
  version :: Bool
  }

data Language = Untyped | SystemF
  deriving (Eq, Show)

cliParser :: Parser CliOptions
cliParser = CliOptions 
  <$> flag Untyped SystemF (long "system-f" <> 
                            short 'f' <> 
                            help "Use the System F interpreter")
  <*> switch (long "version" <> 
              short 'v' <> 
              help "Print the version")

-- Interactive Shell
runShell' :: CliOptions -> IO ()
runShell' CliOptions{version=True} = putStrLn version'
runShell' CliOptions{language=Untyped} = runShell mkShellDesc readlineBackend ()
runShell' CliOptions{language=SystemF} = error "Not Implemented!"

mkShellDesc :: ShellDescription ()
mkShellDesc = shellDesc' $ mkShellDescription commands eval
  where shellDesc' d = d {
          greetingText = Just shellGreeting,
          prompt = shellPrompt
          }

shellGreeting :: String
shellGreeting = "Lambda Calculator (" ++ version' ++ ")\nType :h for help\n"
  
shellPrompt :: s -> IO String
shellPrompt _ = return "λ > "

commands :: [ShellCommand s]
commands = [exitCommand "q",
            helpCommand "h"]

eval :: String -> Sh s ()
eval = either shellPutErrLn' shellPutStrLn' . evalString
  where shellPutErrLn' :: Show s => s -> Sh s' ()
        shellPutErrLn' = shellPutErrLn . show

        shellPutStrLn' :: PrettyPrint s => s -> Sh s' ()
        shellPutStrLn' = shellPutStrLn . prettyPrint

version' :: String
version' = showVersion P.version
 
