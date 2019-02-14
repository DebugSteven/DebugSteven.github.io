---
layout:   post
title:    "Finding Your Space in the Source"
date:     2019-2-13
excerpt:  "My story of getting involved with Rust & open source"
comments: false
tag:
- rust
- open source
- personal
---

I’ve contributed to recently
[x25519-dalek](https://github.com/dalek-cryptography/x25519-dalek),
[curve25519-dalek](https://github.com/dalek-cryptography/curve25519-dalek),
[rustdoc](https://github.com/rust-lang/rust/tree/master/src/librustdoc),
[rust wasm](https://github.com/rustwasm), &
[tower-grpc](https://github.com/tower-rs/tower-grpc)

The short version of my story is this. People in the Rust community are
really nice. Crate owners often label good first issues as such & in my
experience have provided excellence mentorship. I’ve become good friends
with so many people in this community & I think that also plays a part
in why I’m more comfortable getting involved, but I don’t think that’s
necessary to participate!

Without further ado, here is the longer story of my journey with getting
involved in Rust open source.

## RustConf 2017

I attended RustConf in 2017 without having ever written a line of Rust &
attended Rust Bridge! [Ashley Williams](https://twitter.com/ag_dubs) &
[Carol Nichols](https://twitter.com/Carols10cents) walked through making
a web app in Rust. It’s a small project with 1 commit that says,

> cute webapp made during rustbridge

which is objectively true. You can look at it
[here](https://github.com/DebugSteven/rustbridge). We covered how to use
crates, some syntax, & control flow. I remember liking the immutability
by default & pattern matching. I allegedly asked good questions! I made
a few friends & met a lot of really cool people! I was in awe at the
talks, even though I didn’t understand most of it at the time, &
remembered spinning up a new Rust project to play around with
[Rocket](https://rocket.rs/) during the conference.

After I went back home, I went back to self studying Haskell & watched
Rust unfold on Twitter. I did 1 more small, quick project called
[friend-voter](https://github.com/DebugSteven/friend-voter) in November
2017 that parsed an htm file of facebook friends & compared it to a csv
file of potential voters that hadn’t casted their vote yet. The compiler
error messages reminded me of Elm & the nice types reminded me of
Haskell. I really enjoyed writing Rust! After that I went back to
focusing on Haskell because that was what local people around me were
doing at the time.

## denver.rs reactivate!()

Then the Boulder/Denver Rust Meetup rebooted on February 28th, 2018. I
attended! I felt really nervous about showing up to a new tech meet up
because I didn’t think I would belong. That’s how I felt in most tech
spaces up to that point. The Rust Meetup felt almost exactly like
RustConf did. [Pete Lyons](https://twitter.com/focusaurus) & Danny Fritz
created a really welcoming space & I knew I belonged there. I enjoyed
Pete’s talk on learning Rust at the Recurse Center & Joel Dice’s talk on
Tokio. I got a lot of the presentations!

After the talks, there was time allocated to pair programming! I really
like getting to pair with people so I was excited about possibly getting
involved on a project. I got to pair with Pete on a PR to Rust itself!
We wrote some JavaScript to [remember state for the top level collapse
toggle widget](https://github.com/rust-lang/rust/pull/48631) in the
navigation bar to make it nicer to view docs. I wasn’t sure if the
change would be welcome though. What if it wasn’t what the team wanted?
This was my first time doing anything related to open source so I wasn’t
sure how it would go.

Immediately, we were greeted by the rust-highfive bot. The bot said,

> Thanks for the pull request, and welcome! The Rust team is excited to
> review your changes, and you should hear from @steveklabnik (or
> someone else) soon.

The fact that this bot existed to welcome people’s changes was nice. I
felt better that the changes might be well received. We got some code
review from [GuillaumeGomez](https://github.com/GuillaumeGomez), made
another change, & [QuietMisdreavus](https://github.com/QuietMisdreavus)
approved the changes. She even left a comment saying,

> This is great! Thanks for following up on @GuillaumeGomez's comment.
> This is something i've wanted myself, so i'm glad you were able to get
> it rolling.

It was merged! I was excited about participating on something like that.
Since I was new to Rust I wasn’t sure that I could really contribute
something of value in Rust. I got a job writing Rust though & a lot of
the friends I made in that community through Twitter were helping me
learn the language.

## RustConf 2018

I very stubbornly showed up to RustConf last year.

I had a normal day leading up to my flight. I worked, I biked 15 miles,
I did chores, I prepared 2 weeks of Haskell Book Club, & I got on a
plane to go to this conference.

Once I was there I called a friend back home after I got to my hotel in
Portland. I told them,
> why am I at this programming conference when I don’t know how to code?

They replied,
> why are you calling me at midnight telling me things that are untrue?

I told them I wasn’t feeling well. I thought I was having a panic
attack. They suggested I go to the ER. So I did & I had pneumonia.

I’m glad they told me to go because my health stats were Bad. The
doctors initially said that I should remain in the hospital for the
entire weekend. But while I was in the ER, I yelled at the doctors
enough about Rust that they <s>learned about the concept of ownership &
knew that I was merely a mutable borrow whose ownership must be returned
to the caller, RustConf</s> eventually decided to release me. After I
was released, I went directly to the conference at noon & made the
afternoon talks! Shoutout to [Michael
Gattozzi](https://twitter.com/mgattozzi) & [Alex
Payne](https://twitter.com/myrrlyn) for [literally carrying
me](https://twitter.com/DebugSteven/status/1030555170847879168) around
the conference!

You might be wondering,
> why would you put so much effort into going to a conference right
> after you almost died, instead of staying in, resting, & taking care
> of yourself?

I enjoy in person community! It’s how I meet people best! I was excited
to get to listen to the talks & learn new stuff, but I was excited to
meet new people & get to learn what they are working on too! Often
people will have calls for participation or will be looking for
collaborators. Sometimes you’ll talk to someone working on something
cool & you can ask to get involved!

[Sunjay Varma](https://twitter.com/Sunjay03)’s talk on
[Turtle](https://github.com/sunjay/turtle) ended with a call to action
to try out the crate, contribute to the project, & file issues. Ashley
was also looking for new contributors to [rust
wasm](https://github.com/rustwasm) & Michael previously encouraged me to
get involved with the project. On the hack day after the conference, I
made a few small documentation changes. Ashley said that she really
valued the contribution to me in person & that meant a lot to me.

I talked to a few people at the RustConf & we ended up pairing with them
on projects later on!

## Recurse Center

[isis lovecruft](https://twitter.com/isislovecruft) posted a good first
issue for `x25519-dalek` with mentorship on Twitter on the first day of
my batch at RC.

I jumped at the opportunity not only because I think that security is
fascinating, but I got to meet isis & [Henry de
Valence](https://twitter.com/hdevalence) at RustConf & they are both
incredibly kind people. I thought they would be nice to work with & I
had a really good experience contributing to their project!

Security in particular is a difficult area to start contributing to
unless you have a lot of domain specific knowledge. But thanks to Rust,
with its strong types & memory safety, I was able to more easily
implement the desired drop logic & write the new API without worrying that
I would introduce security vulnerabilities or make some other critical
mistake. Henry also provided really good code review that enabled me to
write the API in a way that lined up with the adjoining [RFC for the
x25519 function](https://tools.ietf.org/html/rfc7748#page-7) used for
implementing the Diffie-Hellman key exchange. In a separate post I plan
to write about what all I learned on that project, but the difficulty of
a contribution like this in other languages & communities & the generous
amount of time isis & Henry spent teaching me made this issue stick out
to me.

It was a valuable contribution! Both of them showed that in a number of
ways, but most notably, they listed me as an author of
[`x25519-dalek`](https://crates.io/crates/x25519-dalek) to credit me for
my work.

Since then I’ve done a few other PRs to a couple of other projects. I
got to learn about bits & pieces of projects by working on PRs for these
projects & that’s been a lot of fun! I'm planning on talking about these
contributions, but if you want to see what I've done a few I really
enjoyed doing were:

- [testing the wasm game of life](https://github.com/rustwasm/book/pull/137)
- [implementing a default montgomery point](https://github.com/dalek-cryptography/curve25519-dalek/pull/210)
- [inlining extern crates](https://github.com/rust-lang/rust/pull/57508)
- [retaining protobuf comments](https://github.com/tower-rs/tower-grpc/pull/112)

## Conclusion

The Rust community over & over again has clearly said that beginners
have valuable things to contribute. At the beginning of my batch at the
Recurse Center, I would’ve considered myself a Rust beginner, but I’m
contributing to projects on my own now & helping others with their Rust
projects. The Rust community has helped me level up my ability as a
developer by providing some mentorship to get started & reassurance that
these contributions are wanted & valuable!

I wrote about this because several people asked me how they could get
involved with Rust open source & in open source with the programming
language they choose to use. Some raised concerns about people being
kind & possibly not knowing the language well enough to contribute. I
can only speak about what worked in my experience & I feel like I'm very
lucky to be where I'm at. I found a lot of kind people I enjoy working
with in Rust & I've found lots of people that have helped me learn on
issues. I hope that if anyone decides to contribute to Rust projects
because of this post that is your experience as well!

Nice programmers can be found in every programming community though!
People are also generally receptive to folks that take genuine interest
in the work they are doing. Most people I've encountered so far are
excited about new contributors! Reach out to these people! Send an
email, try to introduce yourself at a meetup, comment on a github issue
on 1 of their projects. See if you can get involved & work with them &
maybe you’ll also find a project you like contributing to in your
programming community.

& remember to be courteous & kind to the people running these projects.
Everyone's time & energy is valuable & it's kind of people to donate
their knowledge, empathy, & time to create open source software. I'm
incredibly grateful to the people that have spent time helping me learn.

Thank you.
