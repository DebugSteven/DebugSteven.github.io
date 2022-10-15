---
layout:   post
title:    "Oh, The Kinds of Errors You'll See"
date:     2018-7-26
excerpt:  "Examples of compiler errors in Haskell & how to fix them"
comments: false
tag:
- beginner
- haskell
---

## Oh, The Kinds of Errors You'll See

Haskell has what I would consider to be 3 different types of errors: parse errors, definition errors, & type errors.

Parse errors occur when we have broken a formatting rule or some convention enforced by the compiler.


Once we fix all of those we will get definition errors.
Definition errors occur when we are calling that function that isn't defined by us in our scope or the function hasn't been imported from another module.


Once we fix all our definition errors we'll get type errors.
Type errors occur when we told the compiler we would do something via our types & we haven't followed through in our function. After you fix all those errors you'll get your program to run! 


We're going to step through fixing a file that has various parse errors & then, at the end, have a file that compiles correctly.
If you would like to follow along in your own REPL you can [find the code here](https://github.com/DebugSteven/ghc-compiler-errors/tree/master/examples/format). 


Here we have a file that's breaking some formatting rules. 
Let's try compiling this & see what error messages we get.

```haskell
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```


#### Error #1
``` 
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:5:1: error: parse error on input ‘module’
  |
5 | module Main where
  | ^^^^^^
Failed, no modules loaded.
```

Our first error upon loading the module is a parse error on input `module`.
This error occurred on line 5 of our file.
Let's go look at it.

```haskell
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where
-- ^ `module Main where` is line 5 of our file
mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

#### Top Of File

The rules, for what the top of Haskell files should look, are:


- Language pragmas are listed at the top of the file
- Module name is declared above imports & code
- Imported modules listed before functions

I personally like to have `LANGUAGE` in all uppercase because I like to yell as much as possible in my code.
If you decide to give your file a module name, it must be above the imported modules & functions, with a capitalized name.
Then you list modules you would like to import into your file. Imported modules must come before your functions. 

#### Fix #1 Order Matters
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

Here we've moved `module Main where` above our imported module.
Let's reload to see our next error.

#### Error #2
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:14:13: error: parse error on input ‘->’
   |
14 |       False -> "no broken rules here... " ++ truth
   |             ^^
Failed, no modules loaded.
```

The compiler tells us that we have a parse error on input right arrow.
It shows us that the error in on line 14.
Let's go check out the code.

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

The error is on the line that starts with `False`, line 14.
But there are a couple of other problems in the `ruleBreaker` & `lie` function too.
So keep in mind the where block & the let for the rules we discuss next.


#### Indentation Rules
For this section, I'm assuming you're using spaces & using whitespace to denote separation of code blocks. 
There are warnings by default in Haskell if you use tabs. You can also use curly braces in your code to denote separate blocks.
Spaces & whitespace are my preference in Haskell because I think it makes my code look nice. :)

- Code implementations start at least 1 space after the function name on the following line

```haskell
rulebraker b = 
    case b of
        ...
```

My most common error is not having enough spaces between my function name 
& my implementation on the next line. You need to have the 
implementation 1 space over on the next line compared to the function name. 
This rule applies for let in expressions, case of expressions, guards, & where blocks!


- New code blocks inside of other functions must be 1 space over to denote a new block

```haskell
    case b of
      True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
```

When you start a new block you need to indent those expressions by at least 1 space.
Most people will use at least 2 spaces for readability. So here we can see that we need
a new code block because of the use of a case of expression. If you're using
a control structure & the following code will be on a new line that's a pretty good
indication you will need to indent your next section.


- Code blocks must spatially align
 
```haskell
rulebraker b = 
    case b of
      True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"
```

Our `True` & `False` here are in the same block because they're both values 
our `case b of` can reduce to. Because these expressions are in the same
block we need to make sure that they line up.

#### Fix #2
Here is our fixed indentation!

```haskell
ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
     "this code will compile fine"
```

Below is our code that wasn't following our indentation rules.

```haskell
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

#### Indentation Fixed
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
     "this code will compile fine"
```
We fixed all our indentation problems by following
the rules we just talked about! Let's reload the
file & see where we're at with this file now.


#### Error #3
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:19:1: error:
    parse error (possibly incorrect indentation or mismatched brackets)
Failed, no modules loaded.
```
Parse error. Possibly incorrect indentation.
I know what you're thinking. 
> You said we fixed all our indentation!


I promise I didn't lie.


This error occurs on line 19 & we don't have a line 19 in our file!
Let's go look at the end of the file instead.


```haskell
let lie = 
     "this code will compile fine"
```
Why doesn't this work? Well, let's look at the rule.


#### Functions are Top Level Declarations
`let` & `where` are meant to define functions
inside other functions to a local scope.


Just a function name at the top level will be fine!


We can't have anything but functions at the top
level of our file. You might be use to declaring
things that look like variables using an identifier
to distinguish it from functions, but in
Haskell everything is a function!

#### Fix #3
Here is the fixed version of our `lie` function.

```haskell
lie = 
  "this code will compile fine"
```

Here is the old version that didn't compile.

```haskell
let lie = 
     "this code will compile fine"
```


#### Functions at The Top Level
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```

We got rid of our `let` here & now let's recompile.


#### Error #4
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:10:1: error:
    The type signature for ‘ruleBreaker’ lacks an 
    accompanying binding
   |
10 | ruleBreaker :: Bool -> String
   | ^^^^^^^^^^^
Failed, no modules loaded.
```

We've moved onto definition errors now! We won't see any more parse errors now!
When you get to this stage in your own files you can congratulate yourself on making through 1 stage of the compilation process. :)


The type signature for `ruleBreaker` doesn't have a function associated with it.
Let's go look at line 10 of our file.


```haskell
ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    ...
```

The type signature & the function name are different.
You may notice that there is a typo in one of them.

If you have a type signature, you must have a function implementation with it.
We'll just fix our typo.

#### Type signatures for functions that exist
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```

We fixed our typo & now let's reload our code!


#### Error #5
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:1:1: error:
    The IO action ‘main’ is not defined in module ‘Main’
  |
1 | module Main where
  | ^
Failed, no modules loaded.
``` 

The next error we get here says that the function `main`
isn't found in our `module Main`. Let's look at the code
& see if that's true.


```haskell
{-# LANGUAGE InstanceSigs #-}
-- | We have a module called Main
module Main where

import Control.Applicative
-- | We have a function called `mymain`
mymain :: Int
mymain = print "hello world"
```

We can see in the code that we have a `module Main` on line 3
& we don't have a function called `main` anywhere in our file.
Instead we have a function called `mymain`.


#### module Main where

The rule we're breaking here is that if you use `module Main`
you have to have a function called `main`. If for whatever reason
you don't want to have a `main` function, just name your module
anything else (that starts with a capital letter).

#### Fix #5 main for Main
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

main :: Int
main = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```

So we will rename `mymain` to `main` & reload our code
to see how we're doing.

#### Error #6
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:8:1: error:
    • Couldn't match expected type ‘IO t0’ with actual type ‘Int’
    • In the expression: main
      When checking the type of the IO action ‘main’
   |
 8 | main = print "hello world"
   | ^

format.hs:8:8: error:
    • Couldn't match expected type ‘Int’ with actual type ‘IO ()’
    • In the expression: print "hello world"
      In an equation for ‘main’: main = print "hello world"
   |
 8 | main = print "hello world"
   |        ^^^^^^^^^^^^^^^^^^^
Failed, no modules loaded.
``` 

We fixed the definition errors we got & now we've gotten some type errors!

Really this is 2 errors, but they go really well hand in hand.
The first error says we couldn't match the expected type of `main`,
which is *IO of something*, with the actual type, in the type signature, `Int`.
The compiler tells us on the 2nd error that we told them we would
give them an `Int`, but we are actually providing an `IO ()`.
It points to line 8, specifically at the expression: 
`print "hello world"`. This is expected because `print` has the type
is `a` to `IO ()` where `a` has the constraint to have an instance of `Show`.
Let's go look at those lines.


```haskell
main :: Int
main = print "hello world"
```

We did in fact say we would return an `Int` here
& we aren't doing it.


### main :: IO Type
`main` always returns `IO` of some type.
Usually `main` gives us back a value of `IO ()`

In our `main` function we use `print`.
`print` has the type `a` to `IO ()`. So we know we'll want to return `IO ()`.

The rule for using the function `main` is that `main` must return `IO` of some type.
It doesn't have to be `IO ()`, but print has the return type of `IO ()`.
Let's change our type signature of `main` to `IO ()`.


#### Fix #6 IO & main, together forever
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

main :: IO ()
main = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```

Here we are, `main` has the type `IO ()`, which reflects
the type that `print "hello world"` gives us. Let's reload.


```
[1 of 1] Compiling Main             ( format.hs, interpreted )
Ok, one module loaded.
```

Sweet! Our file is properly formatted now!
There are no more errors in our code & this will run fine.
So... just one more thing. :)


#### Properly formatted file!
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

main :: IO ()
main = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "it's true!"

lie = 
 "this code won't compile fine"
```

Let's change the strings to reflect that our code compiles haha.

If you notice any issues with this post [please submit an issue here](https://github.com/DebugSteven/DebugSteven.github.io/tree/master/_posts).
If this was easy for you to follow along with please consider adding to the [Haskell wiki on compiler errors](https://wiki.haskell.org/GHC/Error_messages).
