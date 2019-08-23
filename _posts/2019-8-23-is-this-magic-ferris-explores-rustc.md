## Is This Magic!? Ferris Explores rustc

This is the companion guide to the "Is This Magic!? Ferris Explores
rustc" talk presented at RustConf 2019 by QuietMisdreavus and J Haigh.
You can find this post on both of their blogs.

This post is directed at Rust developers who would like to contribute to
the Rust compiler, but don't know where to start. This post is going to
focus on the details of finding a good first issue, getting your
development environment set up, working through an issue, and learning
about the pull request workflow.

## Intro to compiler and crates

### What is the Rust compiler?

To some Rustaceans, the Rust compiler is little more than "the thing
that yells at my code when I run `cargo build`". But there's a complex
world inside the compiler, waiting for an intrepid explorer like
yourself to peer through its depths.

If you're here, that means you're probably interested in the Rust
compiler itself - how it works, how it's structured, and how to get
involved. The Rust compiler is broken down into many crates, which are
roughly arranged into the various *phases* of compilation, where the
compiler takes your code and applies successive steps on it to turn it
into your desired output. Some of the crates in the compiler are:

- `rustc_driver`: Acts as the entry point\*, and orchestrates the compile
  process
- `syntax`: Contains most of the Rust parser, which turns the text of
  your source code into an initial Abstract Syntax Tree (AST)
- `rustc_resolve`: Contains the name resolver, which matches up names in
  procedure code to declarations
- `rustc_incremental`: Contains the incremental compilation engine
- `rustc`: Contains many of the fundamental types in the compiler,
  including the High-level Intermediate Representation (HIR), and the
  code to "lower" it from the AST, as well as the base implementations
  for types and traits
- `rustc_typeck`: Contains the type checking engine
- `rustc_mir`: Contains the types for the Mid-level Intermediate
  Representation (MIR) and the code to construct it from the HIR
- `rustc_codegen_llvm`: Contains the interface to LLVM
- ...and many, many more!

(\*The *actual* entry point is a small file in `src/rustc`, but it
effectively immediately calls a function in `rustc_driver`.)

> Side note: This is why it can sometimes seem like you fix all your
> errors only to find that there were more lurking underneath! It means
> you made it to the next phase.

We're not going into the details of the complete process of compilation
here - there's a lovely set of documentation about the compiler
internals over at <https://rust-lang.github.io/rustc-guide>, which gets
into way more detail than we can fit into a half-hour talk!

## Intro to teams

Instead, let's talk about the *people* who work on the Rust project. The
Rust project is broken down into various *teams*, each with their own
focus area that they work on. There's a team for the internals of the
compiler, a team for language design, a team for tooling that works with
the compiler, a team for the standard library, a team for documentation
and learning material, a team for the infrastructure that keeps the
compiler shipping, a team for community outreach, and loads more.

In addition to teams there are also *working groups* that are either
created by another team with a tighter focus, or were created by the
Project as a whole to focus on improving Rust in a particular niche.
They usually have a specific goal in mind - "land and stabilize
`async`/`await`", "specify a complete grammar for Rust syntax", "set up
a system to translate the Rust website and materials", and so on.

## Finding a Good First Issue

So say you want to jump into it. Probably the first thing you want to do
is find some small thing you can work on, to get your feet wet with the
codebase, the build and test process, and the contribution workflow.
Sure, you can read through the code and build and run the tests without
having something to fix or enhance, but modifying the code makes the
process that much more real.

So how do you get a "good first issue"? There are generally two ways. I
like to refer to them as the "quiet method" and the "social method",
each with variations. The "quiet method" usually involves finding ways
to fix things that people already want, with instructions already
written. The "social method" involves making yourself known to existing
contributors, so that they can walk you through a contribution.

### quiet method #1

The compiler repo puts labels on their issues. Two specific labels that
are good to look out for are `E-easy` and `E-mentor`. These respectively
mean that the necessary change doesn't require much domain-specific
knowledge, or that a contributor has provided instructions (or an offer
for communication) in the issue. Finding both of these on an issue at
the same time is a lucky occurrence - these are precious commodities
that get claimed quickly!

### quiet method #2

Without a doubt, the easiest way to get your name in the contributor
history of the compiler is to fix a typo in the documentation. Typo fix
PRs are easy to make, easy to review, and easy to merge. The docs team
loves these!

### social method #1

> I want to be where the people are...

All the teams generally have spaces where they communicate in semi-real
time. Most teams have a dedicated Discord channel, and the compiler and
language teams even have Zulip instances where discussion can break out
into much more fine-grain threads. Lurking in these spaces can be
invaluable in hitting the ground running when you start writing code.
You'll have the opportunity to get comfortable with the lingo, see
contribution opportunities before they get issues filed, and meet and
greet the teams in their own space.

### social method #2

In the right spaces, simply asking for contribution opportunities can be
enough! Sometimes people have pet issues that they wouldn't fix
themselves, but would happily pass off to new contributors.

This isn't specifically Rust compiler advice. This can work for a lot of
open source projects. After you get involved people may send issues to
you that they think you might enjoy writing code for. Maintainers may
even have issues they'd like fixed that they haven't posted yet and
talking with maintainers can allow you to help discover issues that
hadn't been considered yet!

Failing that, sending a tweet to `@rustlang` on Twitter can suffice, as
they're usually happy to signal-boost people looking to help out in the
community! (Please don't do it all at once during this talk, though, we
don't want to overburden them. `>_>;;`)

### social method #3

Similarly, following the right people on Twitter can make an opportunity
appear. Contributors, like Niko Matsakis, post about new developments in
their domains and new opportunities for contributions.

## Setting up your development environment

Once you've decided on an issue to work on (or even ahead of time, just
for fun), it's time to clone the git repo for the compiler. Before we
can build it, there are a few things to set up first. The build
process has a handful of dependencies, to coordinate the process,
build LLVM, and sync up some required items. Check out the README in
the repo for the complete list, but here's a general list of "build
and test dependencies" for the compiler:

- python
- g++/clang++ (on Windows msvc can also be used)
- make
- cmake
- curl
- git

Typically, you can launch directly into the build process. However,
there are many knobs you can tweak to make the experience nicer. The
build process has its own configuration file, called `config.toml`. This
file is missing by default, with defaults documented in the provided
file `config.toml.example`. If you copy that file to make `config.toml`,
you can start to tweak the settings in the build. Here are a handful of
good ones to keep in mind:

- Set the `build` triple on Windows to force which toolchain to use
- Set `low-priority` to lower the process priority of the build process
- Set `codegen-units` to add parallelism to the Rust build steps
- Set `debug` to enable debug assertions, add debug line info, etc
- Set `incremental` to build incremental artifacts (more disk usage, but faster rebuilds)

## Building and testing code

Fun fact: You don't use `cargo` to build the compiler! Instead you use
the provided `x.py` script, which coordinates the build process of the
compiler. This operates somewhat similarly to `cargo`. In the `x.py`
script also includes special flags to specific building and testing the
compiler. The `x.py` script is actually a shim over the `bootstrap`
binary, also located in the repo. The `x.py` script is also responsible
for sourcing which version of Rust to build the compiler with and
building the `bootstrap` binary itself, among other things. Here
are some example commands for compiling the repo:

```console
$ x.py build
$ x.py build --stage 1
$ x.py build --stage 1 src/libcore
$ x.py build --stage 1 src/tools/rustdoc
$ x.py check
```

The first command builds the compiler, the standard library, and (if
configured in `config.toml`) a handful of default tools. The second one
does something similar, but stops short of providing a *complete* build.
The full details are beyond the scope of this talk, but here's a brief
rundown.

The compiler is built in *stages*. This means the compiler builds itself
multiple times to ensure it works properly. For example, this is
necessary for dynamic linking and proc-macros. If you plan to mess with
codegen or name mangling it's safest to build stage 2 (the default built
for `x.py build`). When in doubt, ask around.

The third and fourth commands show that you can filter specific crates
rather than building "everything". These can be helpful if you're only
working in one part and you want to make sure it builds, or if you're
working on something like rustdoc that isn't part of the default build
process.

Finally, there is an analogue to `cargo check` available to the
compiler. This provides a quick verification that your code *would*
build without going through the entire build process. This is helpful in
the same way that `cargo check` is - if you're in the middle of writing
a fix but want to double-check that some code you wrote parses correctly
or passes borrow-check. It's not necessary to spend the extra time
putting a binary together.

The Rust compiler has an extensive test suite built in, built up over
time as features and bug fixes are added. Here are some sample commands
that run tests in the compiler.

```console
$ x.py test
$ x.py test src/librustc
$ x.py test --stage 1 src/test/run-pass
```

The first one runs *everything* in the test suite - unit tests,
doctests, integration tests, the works. The second runs the unit tests
in the `librustc` crate. The last one builds stage 1 and runs the
integration tests in the `run-pass` directory. There are many kinds of
integration tests in `src/test`, each with their own requirements. The
Rustc Guide has more information!

Another important command to keep in mind is
```console
$ x.py test src/tools/tidy
```

This is a basic tidy tool we have to ensure lines aren't too long,
there's no trailing whitespace, etc. Run this command before opening
your PR.

You can even set up your freshly-built compiler as a toolchain in
Rustup, if you want/need to build some code that doesn't live in a test:

```console
$ rustup toolchain link local /path/to/rust/build/$HOST/stage1
... in some rust project ...
$ cargo +local build
```

Lots and lots of details are in the [rustc guide]!

[rustc guide]: https://rust-lang.github.io/rustc-guide/how-to-build-and-run.html

## Working through an issue

You’ve found an issue, your dev environment is all set up, and you're
ready to start writing code! Awesome!

At this point you’ve probably read through the issue. Make sure you know
the problem you’re trying to solve and ask questions if there’s anything
unclear. Asking questions either on the issue or in the team channel are
the most likely spots to get answers. If you're working with someone or
have been talking with the team you may ask questions somewhere else or
work directly with a mentor. Just ask the team what their preference is.

Let’s walk through an example issue and see about exploring the code and
adding our contribution to it!

panicbit filed an issue that they would to see re-exported crates and
crate items inlined in rustdoc.

This would look like:

```rust
#[doc(inline)]
pub extern crate foo;
```

This code would display `foo` as if it was a module in the docs.

```rust
extern crate foo;
#[doc(inline)]
pub use foo::*;
```

This code would inline the reexported items as if `foo` were a module.

Rustdoc's code lives in the compiler repo, in `src/librustdoc`. We can
find the existing code for how crates and exported items are currently
handled and then change it to how we want it to be!

From a high level, we know rustdoc walks through the generated abstract
syntax tree and then cleans up the AST to remove duplicates. This is
where the implementation for how the docs should be generated lives.
Let’s look at the section of code for crates and crate items.

`src/librustdoc/clean/mod.rs`
```rust
impl Clean<Item> for doctree::ExternCrate {
    fn clean(&self, cx: &DocContext) -> Item {
        Item {
            name: None,
            attrs: self.attrs.clean(cx),
            source: self.whence.clean(cx),
            def_id: DefId { krate: self.cnum, index: CRATE_DEF_INDEX },
            visibility: self.vis.clean(cx),
            stability: None,
            deprecation: None,
            inner: ExternCrateItem(self.name.clean(cx), self.path.clone())
        }
    }
}
```

Our implementation for showing the exported crate and items needs to
go inside the `clean` trait function for an `Item`. We know that we may
return multiple `Item`s since there are many `Item`s that can be
exported from a crate. We want to add `Item` `name`s to a
`please_inline` list if we have the doc inline attribute,
`#[doc(inline)]`, above the `Item`. If the `Item` is in `please_inline`
we then want to iterate through our items and if the `Item` is in
`please_inline`, we want to try to inline that `Item`. Luckily, we also
saw there is a `try_inline` function!

`src/librustdoc/clean/inline.rs`
```rust
pub fn try_inline(
    cx: &DocContext<'_>,
    res: Res,
    name: ast::Name,
    attrs: Option<Attrs<'_>>,
    visited: &mut FxHashSet<DefId>
) -> Option<Vec<clean::Item>> {
    // ...
}
```

There are certain situations where an item can't be inlined (it's
actually local, it's not something we generate a page for, etc), so we
need to take that into account when solving this problem.

This logic ends up looking like this:

```rust
// If there's a `pub` on it, start looking through the attributes
let please_inline = self.vis.node.is_pub() && self.attrs.iter().any(|a| {
    // If there's a `#[doc(...)]` attribute, look through the `(...)`
    a.name() == "doc" && match a.meta_item_list() {
        // If there's an `inline` in the `(...)`, that's what we're
        // looking for
        Some(l) => attr::list_contains_name(&l, "inline"),
        None => false,
    }
});

if please_inline {
    // We need an empty `HashSet` for `try_inline`
    let mut visited = FxHashSet::default();

    // We also need a `Def`, which the compiler uses to identify
    // items the compiler uses. This is a `DefId` for the root of the
    // crate we're importing
    let def = Def::Mod(DefId {
        krate: self.cnum,
        index: CRATE_DEF_INDEX,
    });

    // `try_inline` returns an `Option`. If it returns the crate's
    // items, use that instead of the `ExternCrateItem`.
    if let Some(items) = inline::try_inline(cx, def, self.name, &mut visited) {
        return items;
    }
}
```

The function signature also changes a little bit here. Instead of
returning a single `Item`, the crate, we might return the crate and the
items exported from the crate. We did a few changes to allow us to use
and return a `Vec<Item>`.

Back where this function is called, we replaced our previous use of
`map` with `flat_map`.

`src/librustdoc/clean/mod.rs`

```rust
impl Clean<Item> for doctree::Module {
    let attrs = self.attrs.clean(cx);

    let mut items: Vec<Item> = vec![];
    items.extend(self.extern_crates.iter().flat_map(|x| x.clean(cx)));
    // ...
}
```

We changed the `impl Clean` to be for a `Vec<Item>` instead of a single
`Item`.

`src/librustdoc/clean/mod.rs`

```rust
impl Clean<Vec<Item>> for doctree::ExternCrate {
    fn clean(&self, cx: &DocContext) -> Vec<Item> {
        // ...
    }
}
```

We also used the `vec!` macro to create a vector from our `Item` array
at the end of our implementation of `Clean` on `ExternCrate`.

But we aren’t done yet! We also need to add some new crates to test the
code we just wrote and write some docs to make sure that users can
verify this works and know how to use it in their Rust code! We added a
sample case of this to the test suite.

`src/test/rustdoc/auxiliary/pub-extern-crate.rs`

```rust
#![crate_name = "inner"]
pub struct SomeStruct;
```

`src/test/rustdoc/pub-extern-crate.rs`

```rust
// aux-build:pub-extern-crate.rs
// @has pub_extern_crate/index.html
// @!has - '//code' 'pub extern crate inner'
// @has - '//a/@href' 'inner/index.html'
// @has pub_extern_crate/inner/index.html
// @has pub_extern_crate/inner/struct.SomeStruct.html
#[doc(inline)]
pub extern crate inner;
```

The rustdoc tests are a little special compared to the rest of the
compiler so we're not going to get into the details of how these tests
work. If you're interested you can read more about rustdoc tests
[here](https://github.com/rust-lang/rust/blob/760226733e940cb375f791e894fbb554555eeb01/src/etc/htmldocck.py#L4).

After you’ve looked over your code and tested it and updated relevant
docs it’s time to submit a PR! You can also do a WIP PR if you’d like
feedback as you go. Just want sure to talk with the team involved with
that part of the project.

## PR workflow

All changes to the compiler and tools are coordinated through GitHub
Pull Requests. With a fork on GitHub, you can push up a branch and
create a PR, which will be assigned to and reviewed by a team member.
When you create your PR, it's a good idea to tag the issue it fixes, if
any, and to add a description of the changes you've made. If there is
any additional information that could be helpful to someone reviewing
your PR you should add it here too.

When you post a PR, the `@rust-highfive` bot will assign someone at
random, or you can add `r? @username` to your PR description if you want
to assign someone specific. This can be useful if you've been working
with someone in particular. They may reassign the PR if necessary, by
using the same message.

Finally, when the reviewer looks over your code, they might request
changes or accept/reject your PR. Make any changes, hash discussion out
in chat on the PR or with the team, and bam! You got your contribution!

We also use a bot to coordinate running all the tests on PRs before they
merge. Because of this, when a reviewer is ready to merge your PR, they
don't merge it directly. Instead they'll summon our CI bot, `@bors`, to
add the PR to the testing queue, with `@bors r+`. This makes sure that
your PR passes tests on all our supported platforms before landing in
`master`.

Not every PR will land, but it’s still a contribution of your time and
knowledge and walking away from every issue teach you something new.
It’s awesome to get code in, but you’re always going to walk away with a
contribution.

More information can be found in [CONTRIBUTING.md]!

[CONTRIBUTING.md]: https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md#pull-requests
