# System F
System F is a typed variant of the Lambda Calculus. It is an extension to
another typed &lambda; known as the Simply Typed Lambda Calculus. Additionally,
System F introduces the concept of generic types, also called polymorphic
types [1].

In general, type systems aim to eliminate certain programming errors [3]. A good
type system should reject ill-typed programs, while not being too conservative--
that is, it accepts most valid programs. Many functional programming languages 
are based on type systems similar to System F, including Haskell 
(System FC [4]), and ML (Hindley-Milner [5]).

# Type Context
A typing context is a sequence mapping free variables to their types [2]. Given
a typing context &Gamma;, 

    Γ ⊢ t : T

can be read: under the assumptions &Gamma;, t has type T. When the context is
empty, we may omit &Gamma;.

    ⊢ t : T

To indicate we are adding a mapping to &Gamma;, we use the comma operator.

    Γ, t:T ⊢ v : V

# Simply Typed Lambda Calculus
System F is a straightforward extension of the Simply Typed Lambda Calculus [2],
abbreviated here as &lambda;<sub>&rarr;</sub>. For this reason, we will start 
with &lambda;<sub>&rarr;</sub>.

All terms in &lambda;<sub>&rarr;</sub> have a type. By convention, types are
written beginning with a capital letter. These are all examples of valid
types

    X
    Boolean
    Nat
    NatList

## Function Types
Recall that, in &lambda;, functions take exactly one argument and return a 
value. The types of functions need to indicate the type of the argument and
its return value. 

In order to type abstractions, we introduce the arrow operator, &rarr;. A 
function having the type `A → B` takes an argument of type A and returns a
value of type B. Following are more examples

    1. X → (Y → Z)
    2. (Nat → Nat) → Nat (3)

(1) is a curried function that takes an X and a Y and returns a Z. Finally, (2) 
takes a function of type `Nat → Nat` and returns a Nat.

Function types associate to the right. The following types are equivalent

    X → Y → Z
    X → (Y → Z)

## Syntax
&lambda;<sub>&rarr;</sub>'s syntax is almost identical similar to &lambda;'s. As in &lambda;,
there are three forms

 * Variables
 * Function application
 * Lambda abstraction

### Variables and Function Application
Variables in &lambda;<sub>&rarr;</sub> are identical variables to &lambda;. Variables a just 
names that hold values, as in `x`. To distinguish between these from type variables, we use
names that begin with a lowercase letter.

Function application takes the form `f x y z`, where `f` is an abstraction and `x`, `y`, and
`z` are arguments.

### Lambda Abstraction
The only syntactic difference is in abstractions. In &lambda;<sub>&rarr;</sub>, we have to
specify the type of its argument. Consider the following expression.

    λ x:T. body

This defines a function that takes an argument `x` with type `T`.

# References
1. [System F](https://en.wikipedia.org/wiki/System_F). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages. Benjamin C. Pierce
3. [Type system](https://en.wikipedia.org/wiki/Type_system). Wikipedia: The Free Encyclopedia
4. [System FC: equality constraints and coercions](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC). GHC Developer Wiki
5. [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). Wikipedia: The Free Encyclopedia
