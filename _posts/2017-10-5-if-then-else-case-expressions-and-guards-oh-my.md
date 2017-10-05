---
layout:   post
title:    "if-then-else, case expressions, and guards! Oh my!"
date:     2017-10-5
excerpt:  "A comparison of control structures in Haskell"
comments: true
tag:
- beginner
- haskell
---

My background is C++. Haskell is quite a bit different when it comes to
control structures for directing your program flow. In this post, I want to focus on the difference between if-then-else, case expressions, and guards.


To start off with Haskell has if expressions. This differentiates
itself from an if *statement*. A statement is an action to execute.
That's the smallest standalone line of code in an imperative programming 
language. Haskell doesn't have statements though, only expressions (and
declarations). An expression is a piece of code that is evaluated 
to a value, like a function, which makes sense since Haskell is a 
functional programming language, composed of many functions. This means
you can assign the simplified expression of control structures in Haskell 
directly to variables because they act as functions themselves. 


```haskell
test :: Integral a => a -> a
test g = f g
  where f = if even g then (+) 2 else (+) 3
```

Now that you've got it straight between expressions and statements
let's dig into if-then-else blocks.


An if-then-else block structure is this:
if (A conditional expression evaluated to True or False)
then (An expression returned when the condition is True)
else (An expression returned when the condition is False)


1) The expression you put in your if expression must result in
either True or False.
2) You gotta have an else to handle a False if value.
You must have the else expression. Otherwise you will get a parse error.
Thems the rules.


```haskell
wrong = if True then "this won't work" -- doesn't compile

```


In the example below we can see that the variable woah will always be
equal to "something cool" since the conditional expression True is fully
evaluated and remains True.
```haskell
woah = if True then "something cool" else "something's not working"
```


Here's an example of a function that takes a variable and the condition
changes. If the food variable isn't "pizza" the else expression is
evaluated. Try it out for yourself!
```haskell
party food = if food == "pizza" then "hooray pizza party!" else "it's not pizza :("
```


Case expressions are sorta similar to if-then-else expressions. In fact, we
can rewrite our last example using a case expression instead. 
```haskell
party' food = 
  case food == "pizza" of
    True  -> "hooray pizza party!"
    False -> "it's not pizza :("
```
The condition you want to evaluate goes inbetween the "case" and "of"
and then you can match on the various values that can be returned, in
this instance (==) evaluates to True or False. 


There are couple of differences you probably see here.
1) We match on the possible values our expression can reduce to.
2) We got these cool arrow guys that point to the expression we want to
return based on the matched value or otherwise case.
3) It doesn't have to be a boolean expression. This opens up a lot of
choices for the expression you want to use for a case. Below I did
a silly example of "comparing" the variable food to the string "pizza"
```haskell
compare :: a -> a -> Ordering
```

The possible values of Ordering are LT | EQ | GT 


```haskell
pizza' food =
  case compare food "pizza" of
    LT -> "This food is weaker than pizza"
    EQ -> "Pizza is the correct party food"
    GT -> "This food is greater than pizza!!!!"
```


4) We don't have to match on the cases with the values specifically.
An underscore _ can be used to catch any of the cases we don't handle
specifically. Underscore is kinda like a variable (identifier) in Haskell, 
but you can't assign it to anything... It says "we don't care about this
value." This will make sure our case expression is a complete function,
handling all the different possibilities.

```haskell
pizza'' food =
  case compare food "pizza" of 
    EQ -> "Pizza is the correct party food"
    _  -> "Why are you bringing anything other than pizza to this party"

```


There is a lot more to say for case expressions, but that's the gist of
it. Onto guards!


Guards have some aspects of case expressions and some aspects of
if-then-else. 
They are similar to case expressions because you can have multiple
expressions/values which results in 2 or more possible outcomes.
They are similar to if-then-else expressions because those expressions must 
evaluate to a boolean value.


1) Each expression is preceded by a | and followed by an =
2) The value on the right side of the = is evaluated only if the
expression on the left side is evaluates to True.
3) Instead of using underscore for our catch all (because all our
expressions must evaluate to boolean values), we use otherwise.
otherwise is defined as the value True.


You'd want to use a guard when there are more than 2 or more specific
expressions you want to evaluate with a specific result to return for
each one.

```haskell
party'' :: String -> String
party'' food 
  | food == "pizza"    = "hooray pizza party!"
  | food == "noodles"  = "oooooo we could make ramen"
  | food == "popcorn"  = "popcorn & movie night!"
  | otherwise          = "that's not party food"
```

otherwise is used by convention to make guards more readable. Just to
prove that otherwise is just True we can rewrite our function with the
value instead of the variable.

```haskell
party''' :: String -> String
party''' food 
  | food == "pizza"    = "hooray pizza party!"
  | food == "noodles"  = "oooooo we could make ramen"
  | food == "popcorn"  = "popcorn & movie night!"
  | True               = "that's not party food"
```
