---
layout:   post
title:    "Wrestling with Git & Turkeys"
date:     2018-11-24
excerpt:  "Thanksgiving shenanigans & rust-wasm progress. Week 3 at RC."
comments: false
tag:
- thanksgiving
- friends
- personal
- rust
- git
- open source
---

> I planned Thanksgiving & I’m planning a conference!

<!-- -->
> I wrestled with git & a turkey!

## Turkey Wrestling

The week started with my usual break on Sunday. I went out for breakfast & relaxed as usual. Vaibhav took us to a Damascus bakery & invited the group back to his place for baklava. That was my 1st time having it & it was great! 

It seemed like almost everyone had Thanksgiving plans already. Being at the Recurse Center for me is probably like most people’s experience of going away to summer camp, but in the winter! Some people were traveling home & some people had plans with their family nearby. A few of folks here are from other countries & don’t typically celebrate the holiday. But I’m the kind of person that will put in the effort to make the things I want to exist a reality. I wanted Thanksgiving with friends.

So me, being a reasonable person, decided I would go to the store at 11pm Sunday evening. I bought a 15lb turkey, carried it half a mile home, & up 3 flights of stairs. I made a list of all the other ingredients I needed for all the other recipes & went to the store Wednesday with unexpected efficiency. I invited all the people that wouldn’t normally celebrate as well as everyone else that didn’t already have plans. It turned out great! I planned for about 10 people so that there would be enough for everyone & in total there were 7, so it was perfect. Wednesday & Thursday were spent mostly planning, cooking, cleaning, eating, & hosting! 

I wrote out all the recipes & taped them around the kitchen so that I could cook things in parallel & also concurrently with other people. I prepared the deviled eggs, stuffing, & turkey mostly by myself. The stuffing was about a 4 hour process the night before. There was something very enjoyable about putting the time & effort to try to make all this planned out food. I also found it fascinating to prepare the organs of the turkey for the stuffing. It’s not a part of the animal that I usually cook. I leveled up my cooking knowledge because of that experience. 

My friend Marina helped me with the green beans, mashed potatoes, gravy, crescent rolls, & we made cranberry sauce from scratch! Other friends brought wine, beer, cinnamon rolls, muffins, & pumpkin pie! Everyone chipped in with the cleaning & the kitchen was completely clean before anyone left, which was incredibly kind of everyone. 

Everyone complimented the food & that felt really good. I’ve been told before that I’m not very good at cooking, but when I take my time I can make great things. I think this is a similar phenomenon with coding. The more I practice both of these skills, the quicker I’m getting at both. For now though, working at my own pace to make things & not stressing about my pace is turning out just fine.

Tyler, an old friend of mine from college, showed up to Thanksgiving. We both talked about adjusting to life in New York. I gave him & the room a summary of my life since I graduated college. I told him about the job hunt, Haskell Book Club, co-organizing [DenverFP](https://www.meetup.com/denverfp/) & the [Boulder/Denver Rust Meetup](https://www.meetup.com/Rust-Boulder-Denver), & giving my first talk at a conference. I got to tell my story of [getting pneumonia before RustConf](https://twitter.com/DebugSteven/status/1030555170847879168), the [stubborn bear](https://twitter.com/DebugSteven/status/1036285783454478338) that ate all our food at Crystal Lake & the Boulderite we stumbled upon in the dark when we hiked down the mountain, & my experience at [ICFP]() where people encouraged me to pursue my PhD in programming language theory. It was such an interesting year & I’m excited to lean into more adventures!

And oh yeah, I guess that if I can pull off Thanksgiving in ~3 days I would probably enjoy organizing a conference. So I am! I’m working on plans to create a Rust Conference in Denver, [Colorado Gold Rust](https://twitter.com/COGoldRust). I am excited to tell you more about it soon!

My favorite thing about gatherings of people like that is you get to learn the most interesting things, especially if your group is made of diverse folks with different experiences. This is the reason why I get excited about organizing events & I’m going to do it again!

## Wrestling with WebAssembly & Git

I made a great deal of process on my PR to the rust-wasm project to [add a testing section to the game of life tutorial](https://github.com/rustwasm/book/pull/137). Technically, I already submitted the PR for it. I’m awaiting feedback because I’m sure there are improvements that can be added to it. It was a slow grind on this issue this week. I got stumped by having an older version of wasm & needed to install the new version via `curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh`. It’s a bit quicker than installing wasm via cargo. I also did `rustup update && rustup target add wasm32-unknown-unknown` for good measure & made sure I was using nightly Rust.

I had a bit of a hard time writing the code for this. I had this issue for about 2 weeks, but made most of my progress in about 3 days. I wanted to test the `tick` function & to compare the `cells` of the `input_universe` after one tick to the `cells` of the `expected_universe`. I wanted to put all the functions I thought were associated with the `Universe` type into the `impl Universe` block. 

The `impl Universe` block of code has a `#[wasm_bindgen]` attribute on it. This means that the Rust is compiled to WebAssembly & it’s accessible to the JavaScript code. Apparently you can’t return borrowed values & you can’t pass vectors as arguments. I still don’t totally understand why this is the case, but my guess is it’s likely to do with representing those values in JavaScript & WebAssembly. I would like to understand this a bit better. So some of the functions I expected to put in that block simply could not be implemented there & had to stay as just Rust code only.

I think my best logic error was accidentally trying to compare the pointers for the cells of the different universes instead of the values in the array. I haven’t been using print as a debugging tool for so long that I just stared at my logic & the only error I got was that the test failed. I was convinced that I had made a mistake in my manual calculation. I literally laughed out loud when I printed the values I was comparing only to see that it was memory addresses that would never be the same.

Writing the tutorial while still having questions was difficult. I was happy that I did a little documenting along the way. That made it a lot easier to get started with the writing. I think my biggest concern with writing this tutorial was the concern that people might run into the same issues with the version of rust-wasm. I didn’t want to put a section in testing for that because it was an error with my version, not testing related. I didn’t start at the setup for rust-wasm when I ran through the tutorial again. Someone that starts at the beginning shouldn’t have this problem.

After asking if submitting the PR & getting feedback there was the most convenient way, I ran through what I wrote up one more time. I added some things & added more explanation. I saw that I typoed `firefox` as `firefix`. I didn’t want a completely new commit for this. I tried to do a `git add -u && git commit --amend "message"` to add my new changes & just change the commit message. I didn’t realize that you needed to do a `--force` on a `git push` after you amend a git commit. So then I had merge conflicts & then I created commits I didn’t need. I haven’t used git a whole bunch, but it felt so ridiculous to run into this error. It wasn’t terribly hard to fix though because I was amused with myself rather than frustrated. I did a `git reset HEAD~2`, checked my changes using `git diff`, everything looked good, & I did a `git add -u` & `git commit -m "hooray"`. No, that wasn’t the message, but it was for sure the feeling.
