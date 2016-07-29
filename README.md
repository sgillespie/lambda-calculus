# Lambda Calculus
Lambda Calculus is a tiny functional language for expressing computation based on function abstraction and application [1]. It's ideas form the basis of nearly all functional programming including ML, Haskell, and Scheme [2]. Lambda Calculus is a very small language, and is a good starting point for studying functional programming language design and implementation. 

Lambda Calculus, abbreviated here as &lambda;, has a few features that may seem peculiar at first. To start, all objects are functions. Every function accepts functions as arguments and returns functions [2]. Secondly, pure lambda calculus is untyped. There are several variants that add types, including the Simply Typed Lambda Calculus [3], and System F [4]. Finally, &lambda;'s functions can only take one argument [1].

For example, consider a funcction that adds three numbers

    add x y z = x + y + z


In &lambda;, we would need to compose several functions to simulate a function that takes more than one argument.

    λx. λy. λz. x + y + z
    
As we will see later, this process is called currying [1].

Pure &lambda; is also quite restrictive. You will not find features such as top-level named functions or variables, locally scoped variables, pattern matching, and many other features you might expect of a general purpose programming languages. Fortunately, these extensions are easy to add while preserving the semantics of &lambda;.

# Syntax
&lambda;'s syntax is quite minimal. There are only 3 types of expressions
 * Variables
 * Lambda abstraction
 * Function application
 
Any expression maybe be parenthesized to in order to change the evaluation order. Expressions are evaluated from left to right.
 
## Variables
A variable is just a string of characters that may hold a specific value, just as you would expect. In &lambda;, we typically use the convention of a single lower case letter. For example: `x`, `y`, and `z` are all valid variables.

## Lambda abstraction
A lambda abstraction is an anonymous function definition. As we saw earlier, functions take exactly one argument, and may return another function. Conventionally, we prefix abstractions with a `λ`, taking the following form.

    λv. body
    
Here are a few examples:

    λx. x
    λx. λy. x 
    λf. λx. f x

The first example is the `id` function, it just returns its first argument. The second takes two arguments, and returns the first. This is often used to simulate the value `true`. Finally, the last example applies its first argument to its second argument.

We often omit the inner abstractions to save space. The following two expressions would be equivalent

    λf. λx. λy. f x y
    λf x y. f x y

## Function application
An abstraction is applied by using the form `f x y z`, where `f` is a lambda abstraction and `x`, `y`, and `z` are valid lambda terms. Here are some more examples:

    (λx. x) y
    (λx. x) x
    (λx y. x) (λx. x)

# References
1. [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages, Benjamin C. Pierce
3. [Simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus). Wikipedia: The Free Encyclopedia
4. [System F](https://en.wikipedia.org/wiki/System_F). Wikipedia: The Free Encyclopedia
