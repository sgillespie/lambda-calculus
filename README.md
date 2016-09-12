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

## Natural Numbers
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

# Evaluation
We have now seen how to construct valid &lambda; expressions. We now focus on how to evaluate those expressions. Our process involves identifying **reducible expressions** (*redexes*) and applying conversion rules to them. We do this repeatedly until there are no more redexes. When there are no redexes, the expression is in *Normal Form* [6].

There are three conversion rules [6]:
 * Beta reduction
 * Alpha conversion
 * Eta conversion

## Beta Reduction
&Beta;-reduction is function application. We apply an abstraction `f x. t` to an argument `y` by replacing all free occurrences of `x` in `t` by `y` [3]. Consider an application of `id`

    (λ x. x) y → y
    
We simply copy the body of the abstraction, and replace occurrences of the bound variable `x` with `y`. Consider a slightly more complex example:

    (λ n f x. f (n f x)) (λ f x. x) → λ f x. f ((λ f x. x) f x)
                                    → λ f x. f x
                                    
This is the expression `(succ 0)`, as shown earlier. We apply &Beta;-reduction twice, first on the outermost abstraction, then on the inner abstraction.

### Name Capture
&Beta;-reduction can sometimes produce invalid reductions if great care is not taken. Consider an example where `x` occurs free.

    (λ f x. f x) (λ f. x)
    
If we blindly reduce this, we arrive at a wrong result.

    (λ f x. f x) (λ f. x) → λ x. (λ f. x) x
                          → λ x. x
                          
The free variable `x` is captured by the abstraction `(λ f x. f x)` where `x` is bound. To highlight our mistake, we will rename some variables:

    (λ f x. f x) (λ g. y) → λ x. (λ g. y) x
                          → λ x. y
                          
`y` is now the free variable, and is no longer being discarded. We will use Alpha conversion to avoid captures [6].

## Alpha Conversion
Name captures occur when the free variables of an argument have a parameter of the same name in the body of the abstraction. Using &alpha;-conversion, we change the names of parameter bound by the abstraction [6]. This means that the following expressions are equal.

    (λ x. x)
    (λ y. y)

Returning to our capture example, we first look for captures.

    (λ f x. f x) (λ f. x)
    
Because `x` is free in the argument `(λ f. x)`, we need to use &alpha;-conversion to change the parameter `x` to a unique name.

    (λ f x. f x) (λ f. x) → (λ f y. f y) (λ f. x)
    
We now proceed with &Beta;-reduction.

    (λ f y. f y) (λ f. x) → λ y. (λ f. x) y
                          → λ y. x
                          
## Eta Conversion
&eta;-conversion allows us to further eliminate abstractions if they give the same result for any argument [1]. &eta;-conversion allows us to reduce the following expression

    λ x. f x → f
    
as long as `x` does not occur free in `f`.

# More Examples
We now revisit some extended examples.

## Arithmetic
We now return to Church Encodings. Recall that Church Encodings are a simple way to represent non-negative integers.

    0: λ f x. x
    1: λ f x. f x
    2: λ f x. f (f x)
    3: λ f x. f (f (f x))

Again, these can be all be defined inductively

    zero: λ f x. x
    succ: λ n f x. f (n f x)
    
We now define addition and subtraction [1]:

    add: λ m n f x. m f (n f x)
    multiply: λ m n f. m (n f)
    
For example, consider the expression `2 + 3`. We will solve this using the three &lambda; reduction rules. We begin by expanding `add 2 3` with the definitions above

    add 2 3 → (λ m n f x. m f (n f x)) (λ f x. f (f x)) (λ f x. f (f (f x)))
    
We now apply &Beta;-reduction twice, beginning with the left, outermost application

    → (λ n f x. (λ f x. f (f x)) f (n f x)) (λ f x. f (f (f x)))
    → λ f x. (λ f x. f (f x)) f ((λ f x. f (f (f x))) f x)
    
We will now apply the abstraction `(λ f x. f (f x))` to its first argument, `f`. Notice that `f` is bound by the abstraction, so we must use &alpha;-conversion first

    → λ f x. (λ g x. g (g x)) f ((λ f x. f (f (f x))) f x)

Now that the capture has been resolved, we continue with &Beta;-reduction.

    → λ f x. (λ x. f (f x)) ((λ f x. f (f (f x))) f x)
    
Once again, we apply from the left, outermost abstraction, applying `λ x. f (f x)` to the massive argument `λ f x. f (f (f x))) f x`. However, there's another capture, `x`. We &alpha;-convert that, then &Beta;-reduce it again.

    → λ f x. (λ y. f (f y)) ((λ f x. f (f (f x))) f x)
    → λ f x. (f (f ((λ f x. f (f (f x))) f x)
    
We repeat this process until there are no more redexes

    → λ f x. (f (f ((λ g y. g (g (g y))) f x)
    → λ f x. (f (f (f (f (f x)))))
    
From the sequence above, we know this is 5. Since `2 + 3 = 5`, we have arrived at the correct result.

## Boolean Logic
We can also encode boolean values. These are called Church Booleans [1]

    true: λ t f. t
    false: λ t f. f
    
An `if` statement can be modeled purely with functions [1]

    if: λ p x y. p x y
    
This takes a boolean `p`, an action to take if `p` is true, and an action to take if `p` is false. `If` can be read as if `p` then `x` else `y`.

For our next example, we will examine the following expression:

    λ p. if p 1 0
    
This function takes a boolean `p`, and returns `1` if `p` is true, and `0` otherwise. We now apply this to `true`.

    (λ p. if p 1 0) true → (λ p. (λ q x y. q x y) p (λ f x. f x) (λ f x. x)) (λ t f. t)
    
We then use &Beta;-reduction

    → (λ q x y. q x y) (λ t f. t) (λ f x. f x) (λ f x. x) 
    → (λ x y. (λ t f. t) x y) (λ f x. f x) (λ f x. x)
    → (λ t f. t) (λ f x. f x) (λ f x. x)
    → (λ f. (λ f x. f x)) (λ f x. x)
    → λ f x. f x

As expected, the result is `1`.
    
# References
1. [Lambda Calculus](https://en.wikipedia.org/wiki/Lambd.a_calculus). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages. Benjamin C. Pierce
3. [Simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus). Wikipedia: The Free Encyclopedia
4. [System F](https://en.wikipedia.org/wiki/System_F). Wikipedia: The Free Encyclopedia
5. [Lexcial Scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping). Wikipedia: The Free Encycolopedia
6. The Implementation of Functional Programming Languages. Simon L. Peyton Jones
