# Lambda Calculus
Lambda Calculus is a tiny functional language for expressing computation based on function abstraction and application [1]. It's ideas form the basis of nearly all functional programming including ML, Haskell, and Scheme [2]. Lambda Calculus is a very small language, and is a good starting point for studying functional programming language design and implementation. 

Lambda Calculus, abbreviated here as &lambda;, has a few features that may seem peculiar at first. To start, all objects are functions. Every function accepts functions as arguments and returns functions [2]. Secondly, pure lambda calculus is untyped. There are several variants that add types, including the Simply Typed Lambda Calculus, and System F. Finally, &lambda;'s functions can only take one argument [1].

For example, consider a funcction that adds three numbers

    add x y z = x + y + z


In &lambda;, we would need to compose several functions to simulate a function that takes more than one argument.

    λ x. λ y. λ z. x + y + z
    
As we will see later, this process is called currying.

Pure &lambda; is also quite restrictive. You will not find features such as top-level named functions or variables, locally scoped variables, pattern matching, and many other features you might expect of a general purpose programming languages. Fortunately, these extensions are easy to add while preserving the semantics of &lambda;.

# References
1. [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages, Benjamin C. Pierce
