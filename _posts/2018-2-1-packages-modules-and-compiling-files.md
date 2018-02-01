---
layout:   post
title:    "Packages, Modules, & Compiling Files"
date:     2018-2-1
excerpt:  "Adding packages to projects & loading packages in GHCi"
comments: true
tag:
- beginner
- haskell
---

I recently finished the Building Projects & Testing chapters in the Haskell Book.
I am also working on a project to scrape a website
in Haskell. This means I'll need to use libraries that other people have
written to accomplish scraping, creating a webapp, & testing my
application unless I want to write all that myself (I don't).

I've been having trouble differentiating between Packages & Modules and
where to put them so that my project will compile. I always seem to not
know the module name or have the wrong capitalization, resulting in
projects not compiling. It's not clear if there's a naming convention or
if I'm missing something else. 

## Making a Project Compile

I'm going to use QuickCheck & hspec as examples. 

I'm only covering how to get files to compile, not the 
intricacies of packages & modules.

You've made a new project with stack

`stack new example`

Great! 

Open the `package.yaml` & you'll see a bunch of stanzas generated for
you. We'll want to add our test **packages** to the `tests` stanza.

Here's how that looks:

```
tests:
  example-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - example
    - QuickCheck
    - hspec
```

The next part is adding these libraries to our source files so we can
use them! Since `QuickCheck` and `hspec` are dependencies only for the
`Spec.hs` file we'll want to add our **module** imports there.

Module names do not have to be the same as package names.
We can go the hackage page for [QuickCheck](https://hackage.haskell.org/package/QuickCheck) 
or [hspec](https://hackage.haskell.org/package/hspec) & if you scroll
down to the *Modules* section you can see the exact name with capitalization. 

In our `Spec.hs` file, we'll want to add the imports to the top of the
file. It will look like this (if you keep the default generated code):

```
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = putStrLn "Test suite not yet implemented"
```

To see, run your test suite & download all the packages, you'll run the
`stack test` command in your terminal. It will show all the packages
getting downloaded & the output of the test suite being not implemented.

If you're adding packages to the executable stanza of your
`package.yaml` you'll also just import the modules to the top of your
`Main.hs` file (by default) and to run the project you'll run the `stack
exec example-exe` command. 

## Making a File Compile (Loading Packages in GHCi)

Alright so you're working in just one file. Or maybe you want to experiment a bit with a package. 

_Do you really need to set up a project to get packages in scope?_ 
Nah.

If you have your module imports at the top of your file & want to load
it in GHCi, you'll get an error saying: 

`"Failed to load interface for $MODULE_NAME"` 

for all your modules not in scope.

<br>

Before you load your REPL session, you can run 

`stack ghci QuickCheck hspec` 

or a list of whatever packages you need to load your file.
In your REPL session, you'll need to import the modules you want to use
there too. Then when you try loading the file it will work great!

<br>

If your GHC prompt is using the default settings the modules
will be listed in your session prompt character. 
To prevent this from cluttering up your GHCi session you can either 


` import qualified Test.QuickCheck as Q ` & prefix all your
QuickCheck functions names with `Q.func` like `Q.arbitrary`


or


` :set prompt ">" ` & your prompt will remain ">" for the rest of
your session.

<br>

Loading packages and importing the modules into your GHCi session 
is also really useful for doing development inside the REPL. You can
look at the types of functions in the package or ask for more
information. I've found that developing in smaller chunks in the REPL
leads me to write better functions overall. 
