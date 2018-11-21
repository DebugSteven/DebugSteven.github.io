---
layout:   post
title:    "I'd Work Till I'm Sore"
date:     2018-11-17
excerpt:  "Summary of the 2nd week at RC. Getting back to writing Haskell & being considerate in open source projects."
comments: false
tag:
- personal
- haskell
- rust
- open source
---

By choice, the 12 hour days at RC haven’t really stopped. I did attend some Meetups so there were some days where I spent less time in the physical space, but for the most part it was still spent on tech related events. I did some processing on why I want to spend so much time here. The time I spend here is not only writing code. I stop & talk with people & learn about all sorts of other things. It just feels emotionally really good being here.

The momentum I’ve had learning things here has been amazing though & I want to keep it up! I did a better job this week at walking around & making sure I slept enough & ate enough. For the most part, I think all of those particular issues on self care were an issue of adjusting to NYC. & I am adjusting slowly, but surely.

## Getting Back to Haskell

> I won’t get burned out by writing code for too long; it will be because I check my email too often.

The week started off with an RC breakfast club in Brooklyn run by Vaibhav & a few conversations about Haskell. Talking about Haskell has been hard because I felt a lot of shame over not knowing things & having to ask questions when I was first learning the language. I’m at the Recurse Center in part because I want to regain the ability to ask questions, admit when I don’t know something, & instead feel excited about the opportunity to learn something new! People at RC have expressed a lot of enthusiasm about learning Haskell & that makes me excited. I’ve met a bunch of friendly people that are writing Haskell professionally that are happy to answer questions.

There are so many folks in Haskell that I haven’t met & there are plenty of nice Haskell folks that I already know! I’m at the Recurse Center in part because I want to regain the ability to ask questions & admit when I don’t know something. I picked up the bad behavior of just pretending I know through the shame I felt when people were surprised I didn’t know something when I was learning Haskell over the last year. It stunted a lot of the progress I could’ve made with Haskell. Learning Rust has been a much more pleasant experience even though I’ve been taking a similar kind of approach with learning Rust as I did with Haskell.

I’m trying to restore my feelings of competency in writing Haskell code. I know enough to write practical applications in Haskell, but of course I would love to continue improving. My desire to write Haskell waned a bit due to a slow erosion of my perception of my skills, through unemployment for a bit & frequent unhelpful criticism. I mention this because I believe that this sort of negative feedback, whether intention or not, is a big reason why women & other gender minorities, like myself as a nonbinary person, end up leaving the tech industry. I have to work through these feelings & learn how to navigate this industry while trying to minimize harm to myself & others. There’s hope & I see a lot of people with the same priorities with a commitment to stay around. Everyone I’ve spoken to about this has been really kind about it, suggesting that I come back to it when I’m feeling excited about Haskell again.

Vaibhav helped me get [iHaskell](https://github.com/gibiansky/IHaskell) installed on my machine! I’m pretty excited to use it. He suggested it awhile ago when I asked if there was a way to save sessions in GHCi. He introduced me to a few Haskellers at the Localhost Meetup & I attended Kadena’s Meetup to celebrate 2nd anniversary for the public release of their language for writing small contracts, Pact. They even have an online REPL for you to [try it out](https://pact.kadena.io/), which I thought was pretty cool. All of this has made me really excited to get back to writing Haskell. There are some plans in the works to start a book club & read through [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html) & I know that will be a lot of fun!

## Progress on Rust Open Source

> I need to learn how to be comfortable asking other people I don't know for help.

I made a lot of progress this week on the projects I’m working on. I got a PR merged into [curve25519-dalek](https://github.com/dalek-cryptography/curve25519-dalek) that implements the `Default` trait for `MontgomeryPoint` after some discussion about it. I changed my other open PR to stop using `transmute` to zero the bytes for `SharedSecret` types. `SharedSecret` is a type wrapper for `MontgomeryPoint`. I couldn’t use `clear` previously since there wasn’t a `default` value for `MontgomeryPoint`.

But the other PR was merged into the develop branch & the Travis CI validates PRs on the master branch of the other crates that are dependencies, which makes sense in retrospect. I’ve just left the PR broken for now because I know that curve25519 will have its new changes merged into master soon. I should’ve asked earlier about how the workflow works & waited for the answer. Knowing this now means that maybe it would be helpful to submit a change to the docs to let people know.

I also made a good amount of progress on a PR for the [rust wasm project](https://github.com/rustwasm/team/issues) to add a testing section to the game of life tutorial. I can’t help but think about all the extra progress I would’ve made if I would’ve taken fitzgen’s offer for help up earlier. I ended up writing a bunch about the things I was stuck on, but trying to find a balance between providing too little information & too much information is hard when your communication is asynchronous. I ended up sending clarifying questions about the issue first to make sure I was solving the right problem & then after receiving a response, had a list of things I tried & what issues I encountered, along with a gist of my code, the commands I ran, & the compiler errors I received.

I’ve been anxious about driving into these projects. It’s definitely a bit outside my comfort zone, but it brings me closer to my goals of becoming a better programmer. It’s exciting that I am jumping into these projects & just trying to make small steps forward on them! I’ve been worrying about bothering people, even though everyone has been nice &, while I don’t expect it, people have been very fast in responding to my questions. I worried about my public responses to people on GitHub issues & working group meetings. It’s a combination of coming across professionally, understanding the person that I’m talking to, & hoping my response will be read clearly & as respectful. I want to try to be brief, as to not waste people’s time, but not seem terse due to irritation. It’s a tricky balance & I wish it wasn’t something I worried about as much as I do. I think that the hardest part of contributing to open source for me is making sure that I let people know that I respect them & their work & time.

Contributing to projects written in Rust has appealed because of my positive interactions with the community. Most projects I’ve seen have a code of conduct. Most projects I’ve seen have a guide for contributing & sometimes good first issues. It’s been easier to contribute because the deliberate effort put into making those projects approachable gives me a sense that people would value my help & be kind to me if I put in time & effort into a project.
