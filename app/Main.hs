module Main where

import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Readline (readlineBackend)

main :: IO ()
main = runShell shellDesc readlineBackend ()

shellDesc :: ShellDescription ()
shellDesc = mkShellDescription commands eval

commands :: [ShellCommand s]
commands = [exitCommand "q",
            helpCommand "h"]

eval :: String -> Sh st ()
eval = shellPutStrLn
