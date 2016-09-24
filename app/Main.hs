module Main where

import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Readline (readlineBackend)

import Language.Lambda

main :: IO ()
main = runShell mkShellDesc readlineBackend ()

mkShellDesc :: ShellDescription ()
mkShellDesc = shellDesc' $ mkShellDescription commands eval
  where shellDesc' d = d {
          greetingText = Just shellGreeting,
          prompt = shellPrompt
          }

shellGreeting :: String
shellGreeting = "Lambda Calculus (" ++ version ++ ")\nType :h for help\n"
  
shellPrompt :: s -> IO String
shellPrompt _ = return "λ > "

commands :: [ShellCommand s]
commands = [exitCommand "q",
            helpCommand "h"]

eval :: String -> Sh s ()
eval = either shellPutErrLn' shellPutStrLn' . parseExpr
  where shellPutErrLn' :: Show s => s -> Sh s' ()
        shellPutErrLn' = shellPutErrLn . show

        shellPutStrLn' :: Outputtable s => s -> Sh s' ()
        shellPutStrLn' = shellPutStrLn . ppr

version :: String
version = "0.1.0"
 
