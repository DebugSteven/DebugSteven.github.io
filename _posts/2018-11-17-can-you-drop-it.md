---
layout:   post
title:    "Can you Drop it?"
date:     2018-11-17
excerpt:  "Resource allocation & the implementation of drop logic in Rust"
comments: false
tag:
- rust
- open source
- security
- compiler
- programming languages
---

> I have no idea how to implement `std::mem::drop`

<!-- -->
> You don't; `mem::drop` is nothing.

Last week I was working on an issue for [Dalek](https://dalek.rs/) that requested some new types to wrap around `[u8; 32]` types used in the code for different kinds of secret keys & to clear the value of the secret key when the value is dropped.

I hadn’t thought a lot about `drop`. For the last year, when I was writing Haskell or writing web applications I didn't think about resource management. Also, because of the ownership semantics in Rust, when the program is done executing it automatically cleans up its resources & `drop` is automatically called when a value is no longer in use.

## What are we dropping?

Honestly, off the top of my head, I wasn't sure to implement `drop`. I thought about what the purpose of `drop` really is. It's to deallocate resources that we know we aren't using anymore. The `drop` function just needs to indicate to the compiler that we don't need the resources it has kindly allocated for us anymore so the operating system can allocate those resources for other processes.

I want to say more on what I mean by "resources." At first, I thought that the only sort of resource we would deallocate in a program was a piece of memory. Most often memory is the resource you will deallocate when `drop` is called because it's where our values from our functions live when we are executing our code. There are plenty of other kinds of resources that our programs use though! We have file handlers; we have network connections, process connections, all kinds of state that our program knows about that isn't a memory address.

## How do we drop?

Then I looked at the type signature & thought about what it meant.
```rust
pub fn drop<T>(_x: T)
```
This type signature says, we have a variable `x` that we never use (which we indicate by prefixing the variable with an underscore) that has a generic type `T`, where `T` has an implementation of the `Drop` trait. The return type for this function is an implicit `()`. My guess was the simplest implementation of a function that evaluates to `()` & doesn't use its only argument would be empty curly braces.

So I looked up the implementation for `drop` & found... *drumroll*
```rust
{ }
```
These good empty curly braces. A Rust expression that evaluates to `()`. To me at least, this made sense. We are taking ownership of the value `x`, moving it into nothing, & giving the program back a `()` at the type level to express that operation.

There are [rules for the order in which resources are dropped](https://doc.rust-lang.org/std/ops/trait.Drop.html).
* Variables are dropped in the reverse order that they are declared.
* We drop fields recursively, the outer most type is dropped first to the inner most type being dropped last.

The drop checker determines the order that values will be dropped. The compiler & type system alone do not have enough information about the contents of a type to know if we might accidentally create a dangling pointer if we drop resources potentially too early when our rules for ownership might allow references to those references later on in the code.

## Which kinds of values can we not drop?

#### Any value that we can't take ownership of cannot be dropped.

Anything with a `Copy` trait cannot be dropped. With `Copy`, a bitwise copy of your value is made implicitly when it is assigned to another variable or passed to a function & your original value is still valid. You can still call `drop` on values that have the `Copy` trait. What you'll be doing is moving a copy of that value when you pass it to `drop` & then dropping the copy. The value will still exist after. This is because copy semantics work differently than the default move semantics with regards to ownership.

```rust
fn main() {
    let x: u8 = 1; // primative types have an implementation for Copy
    drop(x); // a copy of `x` is moved and dropped

    println!("x: {}"); // outputs: x: 1
}
```

Anything with a `Copy` trait cannot have an implementation for the `Drop` trait
If you try to write an implementation for `Drop` for a type that has an instance of `Copy` you'll end up with something like this:

```rust
#[derive (Debug, Copy)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) { }
}

fn main() {
    let x = Foobar(1);
    println!("x: {:?}", x);
    drop(x);
}
```

```text
error[E0184]: the trait `Copy` may not be implemented for this type; the type has a destructor
 --> src/main.rs:1:18
  |
1 | #[derive (Debug, Copy)]
  |                  ^^^^ Copy not allowed on types with destructors
```

If you delete the `Copy` in the `derive` attribute in the code sample then the code will compile.

#### Values with generic lifetimes that could be destroyed before they would be referenced

This goes back to the rules about the order that resources are dropped. Values are dropped in the opposite order that they are declared & they are dropped from outer most type to insider most type.

If we have types with generic lifetimes, it's possible that we could have an implementation for `drop` that tries to use a value that has already been dropped. Since we don't know what the logic inside an implementation of `drop` might do or reference there has to be extra care that we don't access memory that has already been deallocated.

There's a great explanation along with examples of the [extra considerations when implementing drop with generic lifetimes](https://doc.rust-lang.org/nomicon/dropck.html) that I found really interesting to read about.

## Can we drop it?

Sometimes we want to do something with our value before we tell the compiler that we're done using a resource. In this particular case, I wanted to clear the values before deallocating the memory. Memory isn't zeroed when it is deallocated. We wouldn't want to leave sensitive information in memory in the event that another process were able to get to it.

```rust
use clear_on_drop::clear::Clear;

/// A Diffie-Hellman Ephemeral key.
#[repr(C)]
#[derive(Default)] // we derive Default in order to use the clear() method in Drop
pub struct Ephemeral(pub (crate) Scalar);

// A Scalar is represented as
// pub struct Scalar{ pub(crate) bytes: [u8; 32]};

/// Overwrite ephemeral key material with null bytes when it goes out of scope.
impl Drop for Ephemeral {
    fn drop(&mut self) {
        self.0.clear();
    }
}
```

In our instance of `drop` for `Ephemeral`, we can call the `clear` method from the [`clear_on_drop` crate](https://crates.io/crates/clear_on_drop) on the `Scalar` value. `clear` requires that an implementation for `Default` so there's a value to use to overwrite the memory location with. In this case, we get an `Ephemeral` value that is overwritten as 32 zero bytes. At the end of the function scope, the value for `Ephemeral` is dropped.

Here is an instance where we don't have a default value.
```rust
/// A Diffie-Hellman SharedSecret
pub struct SharedSecret(pub (crate) MontgomeryPoint);

// A MontgomeryPoint is represented as
// pub struct MontgomeryPoint(pub [u8; 32]);

/// Overwrite shared secret material with null bytes when it goes out of scope.
impl Drop for SharedSecret {
    fn drop(&mut self) {
        let bytes: &mut [u8; 32] = unsafe {
            mem::transmute::<&mut MontgomeryPoint, &mut [u8; 32]>
            (&mut self.0)
        };
        bytes.clear();
    }
}
```

The major difference in this `drop` implementation is that we are using [`transmute`](https://doc.rust-lang.org/std/mem/fn.transmute.html). From the documentation,
> `transmute` is semantically equivalent to a bitwise move of one type into another. It copies the bits from the source value into the destination value, then forgets the original. So the result is essentially the same. We get a `MontgomeryPoint` value that is overwritten as 32 zero bytes & at the end of the function scope, the value for `MontgomeryPoint` is dropped.

I imagine that this is just scratching the surface when it comes to `drop` & useful logic that can modify your resources before they are dropped. These few examples for security were really cool & I'm excited to see more examples.
