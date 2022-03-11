# Introduction
The Lambda Calculus can easily be extended with a static type system. We 
explore two such systems, the *Simply Typed Lambda Calculus* and *System F*. 
The Simply Typed Lambda Calculus, abbreviated as &lambda;<sub>&rarr;</sub>, is 
the simplest typed &lambda;. *System F* is an extension of 
&lambda;<sub>&rarr;</sub> that introduces the concept of generic types, also 
called polymorphic types [1].

In general, type systems aim to eliminate certain programming errors [3]. A 
good type system should reject ill-typed programs, while accepting most valid 
programs [2]. Many functional programming languages are based on type systems 
similar to System F, including Haskell (System FC [4]), and ML (Hindley-Milner 
[5]).

# Type Context
A typing context is a sequence mapping free variables to their types [2]. Given
a typing context &Gamma;,

    Γ ⊢ t : T

can be read: under the assumptions `Γ`, `t` has type `T`. When the context is
empty, we may omit `Γ`.

    ⊢ t : T

To indicate we are adding a mapping to `Γ`, we use the comma operator.

    Γ, t:T ⊢ v : V

# Simply Typed Lambda Calculus
All terms in &lambda;<sub>&rarr;</sub> have a type. By convention, types begin
with a capital letter. These are all valid types

    X
    Boolean
    Nat
    NatList

## Function Types
In order to type abstractions, we introduce the arrow operator, &rarr;. A
function having the type `A → B` takes an argument of type `A` and returns a
value of type `B` [7]. Following are more examples

    1. X → (Y → Z)
    2. (Nat → Nat) → Nat

(1) is a curried function that takes an `X` and a `Y` and returns a `Z`. 
Finally, (2) takes a function of type `Nat → Nat` and returns a Nat.

Function types associate to the right. The following types are equivalent

    X → Y → Z
    X → (Y → Z)

## Syntax
As in &lambda;, &lambda;<sub>&rarr;</sub> has three forms [2]

 * Variables
 * Function application
 * Lambda abstraction

Variables and applications in &lambda;<sub>&rarr;</sub> are identical to those
in &lambda; [2]. The only syntactic difference is in abstractions [1].

### Variables
A variable is just a name that holds a specific value. 

    x
    myVar

To distinguish these from type variables, we use names that begin with a 
lowercase letter.

### Function Application
Function application takes the form 

    f x y z
    
where `f` is an abstraction and `x`, `y`, and `z` are arguments.

### Lambda Abstraction
Recall that, in &lambda;, functions take exactly one argument. In 
&lambda;<sub>&rarr;</sub>, we must specify the type of that argument. Consider 
the following expression.

    λ x:T. body

This defines a function that takes an argument `x` with type `T`.

## Examples
Church Numerals can be used in &lambda;<sub>&rarr;</sub> just as they are in
&lambda;. In &lambda;, the Church Numerals are defined by

    0: λ f x. x
    1: λ f x. f x
    2: λ f x. f (f x)
    3: λ f x. f (f (f x))

In order to translate these expressions to &lambda;<sub>&rarr;</sub>, we need
to add types to the arguments `f` and `x`. The type for `x` can be any type,
so we assume there is a type `CN`. The type for `f`, the type will be
`CT → CT`. We can now construct the numerals.

    0: λ f:(CN → CN) x:CN. x
    1: λ f:(CN → CN) x:CN. f x
    2: λ f:(CN → CN) x:CN. f (f x)
    3: λ f:(CN → CN) x:CN. f (f (f x))

## Type Checking
In order to typecheck a &lambda;<sub>&rarr;</sub> expression, we need to
determine its type. If we can determine an expressions type, then it is
*well-typed*.

There are three typing rules, one for each form; that is, variables,
abstractions, and applications.

### Variables
We use the following typing rule for variables.

    x:T ∈ Γ
    ⇒ Γ ⊢ x:T

In plain english, if the context `Γ` contains `x` of type `T`,  then `x` has
type `T` in the context `Γ`.

### Abstractions
Abstractions use the following typing rule.

    Γ, x:T ⊢ y:U
    ⇒ Γ ⊢ λ x:T. y : T → U

We first add `x:T` to the context `Γ`. If `y` has type `U` in this context,
then `λ x:T. y` has type  `T → U`

### Applications
Finally, function applications have the following rule.

    Γ ⊢ x:T → U
    Γ ⊢ y:T
    ⇒ Γ ⊢ x y : U

If `x` has type `T → U` and `y` has type `T` in the context `Γ`, then `x y`
has the type `U`.

## Typing Examples
Returning to Church Numerals, consider the representation of `1`.

    λ f:(CN → CN) x:CN. f x

We start with an empty context.

    Γ = {}

We then add `f` and `x` to the context.

    Γ = {f:(CN → CN), x:CN}

Using the typing rule for abstractions, we know the type is

    Γ ⊢ λ f(CN → CN) x:CN. f x : (CN → CN) → CN → ?

and `?` is the type of the body of the function. We now examine the body.

    f x

We apply the applications rule

    Γ ⊢ f x : CN

Therefore,

    Γ ⊢ λ f(CN → CN) x:CN. f x : (CN → CN) → CN → CN

In fact, all numerals have the same type.

## Limitations
&lambda;<sub>&rarr;</sub> does support generic types. This means that you
cannot define functions that can operate on multiple types. For example,
consider the identity function. In &lambda;, it's defined as

    λ x. x

In &lambda;<sub>&rarr;</sub> you would have to redefine it for each type you
want to use it with

    λ x:CN. x
    λ x:Boolean. x
    λ x:((CN → CN) → CN → CN). x

This violates the DRY principal (Don't Repeat Yourself). Fortunately, System
F addresses this.

# System F
As mentioned earlier, System F is an extension of &lambda;<sub>&rarr;</sub>.
System F introduces the concept of *polymorphic types*. Using polymorphic types,
we can define our functions generically. We use type variables in place of
actual variables, and instantiate them with concrete types when necessary [1].

## Types
Types in System F can be

 * Simple types
 * Type variables
 * Universal types

Simple types are all types introduced by &lambda;<sub>&rarr;</sub>. *Type
variables* are names that can represent actual types. By convention, we use
names that start with capital letters for types and type variables.

*Universal types* are synonymous with polymorphic types. Universal types take
the form

    ∀ X. T

This can be read: for all types `X`, `T`. The following type expressions are
all valid.

    ∀ X. X → X
    ∀ X Y. X → Y → X
    (∀ X. X → X) → (∀ Y. Y → Y)

## Syntax
In addition to the syntax rules of &lambda;<sub>&rarr;</sub>, System F
introduces two new ones. We repeat each of the syntax rules here.

### Variables
Once again, variables are names that hold values. By convention, variables begin
with a lowercase letter to distinguish them from type variables. These are all
valid examples of variables:

    x
    y
    myVar

### Abstraction
Abstractions in System F are functions that take one argument. Just as in
&lambda;<sub>&rarr;</sub>, they take the form

    λ x:T. body

This defines a function that takes an argument `x` with type `T`.

### Function Application
Function application takes the form

    f x y z

where `f` is an abstraction and `x`, `y`, and `z` are arguments.

### Type Abstraction
In System F, polymorphism is achieved by using type variables and instatiating
them only when necessary. Type variables are introduced by *type abstracions*.
Type abstractions behave like regular abstractions, except that they operate on
types.

Type abstractions take the form

    Λ X. body

This defines a type function that takes an argument `X`.

### Type Application
*Type application* instantiates a type abstraction to a concrete type. It
behaves like functional application, except that it operates on types. A type
application takes the form

    t [T]

Here the expression `t` is applied to the type `T`.

## Examples
We return to Church Numerals. In &lambda;<sub>&rarr;</sub> we defined these
by

    0: λ f:(CN → CN) x:CN. x
    1: λ f:(CN → CN) x:CN. f x
    2: λ f:(CN → CN) x:CN. f (f x)
    3: λ f:(CN → CN) x:CN. f (f (f x))

Given a type `CN`. While this is perfectly valid in System F, we can also
generalize the type `CN`, with a type abstraction.

    0: Λ X. λ f:(X → X) x:X. x
    1: Λ X. λ f:(X → X) x:X. f x
    2: Λ X. λ f:(X → X) x:X. f (f x)
    3: Λ X. λ f:(X → X) x:X. f (f (f x))

We use a type abstraction to introduce a polymorphic type `X`. Using type
application, we can instantiate these to the concrete type `CN`. Here

    0: (Λ X. λ f:(X → X) x:X. x) [CN]
    1: (Λ X. λ f:(X → X) x:X. f x) [CN]
    2: (Λ X. λ f:(X → X) x:X. f (f x)) [CN]
    3: (Λ X. λ f:(X → X) x:X. f (f (f x))) [CN]

Using a combination of type abstraction and application, we define functions
that can operate on all types.

## Type Checking
Just like &lambda;<sub>&rarr;</sub>, an expression is *well-typed* if we can
determine its type.

In addition to the typing rules in &lambda;<sub>&rarr;</sub>, we introduce two
more. We repeat them here.

### Variables
Variables have the typing rule

    x:T ∈ Γ
    ⇒ Γ ⊢ x:T

If the context `Γ` contains `x` of type `T`,  then `x` has type `T` in the 
context `Γ`.

### Abstractions
Abstractions use the typing rule

    Γ, x:T ⊢ y:U
    ⇒ Γ ⊢ λ x:T. y : T → U

We first add `x:T` to the context `Γ`. If `y` has type `U` in this context,
then `λ x:T. y` has type  `T → U`

### Applications
Function applications have the rule

    Γ ⊢ x:T → U, Γ ⊢ y:T
    ⇒ Γ ⊢ x y : U

If `x` has type `T → U` and `y` has type `T` in the context `Γ`, then `x y`
has the type `U`.

### Type Abstractions
Type abstraction is the first of two new rules.

    Γ, X ⊢ t:T
    ⇒ Γ ⊢ Λ X. t : ∀ X. T

We add the type `X` to the context. If `t` has type `T`, then `Λ X. t` has the
type ∀ X. T.

### Type Applications
Finally, type applications have the rule

    Γ ⊢ t : ∀ X. T
    ⇒ Γ ⊢ t [U] : [X ↦ U] T

Assume `t` has type `∀ X. T`. Given the type application `t [U]`, we substitute
all occurrences of `X` with `U` in `T`.

## Typing Examples
We return to Church Numerals. Consider the representation of `1`.

    Λ X. λ f:(X → X) x:X. f x

We start with an empty context.

    Γ = {}

We then add `X` to the context.

    Γ = {X}

Using the typing rule for type abstractions, we know the type is

    Γ ⊢ Λ X. λ f:(X → X) x:X. f x : ∀ X. ?

and `?` is the type of the body of the type abstraction. We add `f` and `x` to
the context.

    Γ = {X, f:X→X, x:X}
    
Then apply the rule for abstractions,

    Γ ⊢ λ f:(X → X) x:X. f x : (X → X) → X → ?

and `?` is the type of the body of the function. The body is

    f x

We apply the applications rule

    Γ ⊢ f x : X

We now know the type of the abstraction

    Γ ⊢ λ f:(X → X) x:X. f x : (X → X) → X → X

and the entire expression

    Γ ⊢ Λ X. λ f:(X → X) x:X. f x : ∀ X. (X → X) → X → X

# References
1. [System F](https://en.wikipedia.org/wiki/System_F). Wikipedia: The Free Encyclopedia
2. Types and Programming Languages. Benjamin C. Pierce
3. [Type system](https://en.wikipedia.org/wiki/Type_system). Wikipedia: The Free Encyclopedia
4. [System FC: equality constraints and coercions](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC). GHC Developer Wiki
5. [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). Wikipedia: The Free Encyclopedia
6. [Lambda Calculus](https://github.com/sgillespie/lambda-calculus/blob/master/doc/lambda-calculus.md). Sean Gillespie
7. [Simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus). Wikpedia. The Free Encyclopedia
