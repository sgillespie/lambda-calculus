# Lambda Calculus
Lambda Calculus is a tiny functional language for expressing computation based on function abstraction and application [1]. Its ideas form the basis of nearly all functional programming languages including ML, Haskell, and Scheme [2]. Lambda Calculus is a very small language, and is a good starting point for studying functional programming language design and implementation. 

Lambda Calculus, abbreviated here as &lambda;, has a few features that may seem peculiar at first. To start, all objects are functions. Every function accepts functions as arguments and returns functions [2]. Secondly, pure &lambda; is untyped. There are several variants that add types, including the Simply Typed Lambda Calculus [3] and System F [4]. Finally, &lambda;'s functions can only take one argument [1].

For example, consider a function that adds three numbers

    add x y z = x + y + z


In &lambda;, we would need to compose several functions to simulate a function that takes more than one argument.

    λx. λy. λz. x + y + z
    
As we will see later, this process is called currying [1].

Pure &lambda; is also quite restrictive. You will not find features such as top-level named functions or variables, locally scoped variables, pattern matching, or many other features you might expect of a general purpose programming languages. Fortunately, these extensions are easy to add, while preserving the semantics of &lambda;.

# Syntax
&lambda;'s syntax is quite minimal. There are only 3 types of expressions [2]
 * Variables
 * Lambda abstraction
 * Function application
 
Any expression maybe be parenthesized in order to change the evaluation order. Expressions are evaluated from left to right [6].
 
## Variables
A variable is just a name that may hold a value [2], just as you would expect. In &lambda;, we typically use the convention of a single lower case letter. For example: `x`, `y`, and `z` are all valid variables.

## Lambda abstraction
A lambda abstraction is an anonymous function definition [6]. As we saw earlier, a function takes exactly one argument, and may return another function. Conventionally, we prefix abstractions with a `λ`, taking the following form.

    λv. body
    
Here are a few examples:

    λx. x
    λx. λy. x 
    λf. λx. f x

The first example is the `id` function, it just returns its first argument. The second takes two arguments, and returns the first. This is often used to simulate the value `true`. Finally, the last example applies its first argument to its second argument.

## Function application
An abstraction is applied by using the form `f x y z`, where `f` is a lambda abstraction and `x`, `y`, and `z` are valid lambda terms. Here are some more examples:

    (λx. x) y
    (λx. x) x
    (λx. λy. x) (λx. x)
    
## Bound and Free Variables
&lambda; scoping rules are similar to lexical scoping [5]. An occurrence of a variable `x` is **bound** when it occurs in the body `t` of an abstraction `λ x. t`. An occurrence of a variable is **free** when it is not bound by an abstraction [2]. For example, in the expression `λ x. x`, `x` is bound by the enclosing abstraction. In the expression `λ y. f y`, `f` occurs free while `y` is bound.

## Currying
&lambda; does not include multi-argument functions. Instead, an abstraction can only take one argument [2]. To simulate multi-argument functions, we use functions that return other functions, each taking one argument. We saw an informal example earlier. Let's consider a more concreate example,

    λf. (λx. f x)
    
Here, we create a function that applies the first argument to its second. This process is called **currying** [1].
    
We often omit the inner abstractions to save space. The following two expressions are equivalent

    λf. λx. λy. f x y
    λf x y. f x y

# Example
&lambda; is surprisingly expressive for a language so small. We will now look at representing the set of nonnegative integers using pure &lambda;. These ideas can be used to represent nearly any data type, including booleans, records, and algebraic data types.

# Natural Numbers
Natural numbers can be achieved by using what we call Church Numerals [1].

    0: λ f x. x
    1: λ f x. f x
    2: λ f x. f (f x)
    3: λ f x. f (f (f x))

Any number `n` is achieved by applying it's first argument, `f`, `n` times, to its second argument, `x`. We can generalize this to two functions

    zero: λ f x. x
    succ: λ n f x. f (n f x)
   
We have defined natural numbers inductively, using two cases. Zero is defined exactly as we did earlier, and it's successor, as n + 1. Every natural can be composed using those two functions. For example, we can achieve the number 2, by using only zero and successor.

    2 = succ (succ zero) 
      = (λ n f x. f (n f x)) ((λ n f x. f (n f x)) λ f x. x) 
      = λ f x. f (f x)


# References
1. [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages. Benjamin C. Pierce
3. [Simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus). Wikipedia: The Free Encyclopedia
4. [System F](https://en.wikipedia.org/wiki/System_F). Wikipedia: The Free Encyclopedia
5. [Lexcial Scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping). Wikipedia: The Free Encycolopedia
6. The Implementation of Functional Programming Languages. Simon L. Peyton Jones
