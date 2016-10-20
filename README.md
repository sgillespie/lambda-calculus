# Lambda Calculus
Lambda Calculus is a tiny functional language for expressing computation based 
on function abstraction and application. 

This project is intended to be an educational resource for learning and 
implementing functional programming languages.

# Introduction to Lambda Calculus
An introduction to Lambda Calculus is provided. The tutorial can be found at

[docs/lambda-calculus.md](docs/lambda-calculus.md)

## Extended Examples
More examples can be found in the test-suite, located at

[test/Language/Lambda/Examples](test/Language/Lambda/Examples)

# Implementation
A simple implementation of lambda calculus is included. It is written in Haskell
and is implemented to be as easy to follow, at the possible expense of performance.

## Building
In order to build, you will need

 * GHC
 * cabal-install
 * stack

Build:

    stack build
    
Then install:

    stack install
    
## Running
Once the program is installed, you simply run it:

    lambda-calculus
    
This will open a repl (read-eval-print loop) prompt

    Lambda Calculus (0.2.0)
    Type :h for help

You can start typing lambda calculus expressions and the program will evaluate them
and print the result. Here are a few examples:

    Lambda Calculus (0.2.0)
    Type :h for help
    λ > \x. x
    λx. x
    λ > (\x. x) n
    n
    λ > (\n f x. f (n f x)) (\f x. f (f x))
    λf x. f (f (f x))
    λ > :q
    
You can exit by typing the command :q.

## Running Tests
In order to run the testsuite, run

    stack test
    
# Author
**Sean Gillespie** [sean@mistersg.net](sean@mistersg.net)

# License
This project is licensed under the MIT License. See [LICENSE](LICENSE)
