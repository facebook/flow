---
layout: guide
---

The Flow community is growing rapidly, with thousands of software developers using Flow, teaching others about Flow, and sharing things that are useful to other Flow developers. We can't possibly link to every interesting video or tutorial, but if you're interested in learning more about Flow, here is some related stuff you might want to check out.

## Videos <a class="toc" id="toc-videos" href="#toc-videos"></a>

### The Fundamentals Of Flow <a class="toc" id="toc-the-fundamentals-of-flow" href="#toc-the-fundamentals-of-flow"></a>

In this talk, [Alex Booker](https://twitter.com/bookercodes) delivers a fast-paced introduction to Flow that focuses on the fundamentals. Alex's explanation is clear and concise so it's a great way to get started if you're thinking about trying out Flow.

<iframe width="560" height="315" src="https://www.youtube.com/embed/xWMuAUbXcdQ" frameborder="0" allowfullscreen></iframe>

### A Deep Dive Into Flow <a class="toc" id="toc-a-deep-dive-into-flow" href="#toc-a-deep-dive-into-flow"></a>

Here, [Jeff Morrison](https://twitter.com/lbljeffmo) of the Flow team takes a deep dive into the heart of how Flow works. This talk does stay conceptual rather than academic, so you don't have to be familiar with type system theory to appreciate this talk. If you watch this, you will learn something.

<iframe width="560" height="315" src="https://www.youtube.com/embed/VEaDsKyDxkY" frameborder="0" allowfullscreen></iframe>

### Flow: Static Type Systems At Scale <a class="toc" id="toc-flow-static-type-systems-at-scale" href="#toc-flow-static-type-systems-at-scale"></a>

[Avik Chaudhuri](https://github.com/avikchaudhuri) announced the open source Flow project at the [@Scale conference](https://atscaleconference.com/), explaining why Facebook needed Flow and giving an overview of the core features. This is also a great explanation of the guiding principles behind Flow: don't slow developers down, and don't limit their freedom.

<iframe width="560" height="315" src="https://www.youtube.com/embed/M8x0bc81smU?start=765" frameborder="0" allowfullscreen></iframe>

## Research Papers <a class="toc" id="toc-research-papers" href="#toc-research-papers"></a>

Flow relies on several pieces of academic type systems research for its fast and powerful type inference, especially to help it run quickly on large code bases. If you're interested on getting even deeper into the theory behind Flow, you can check out these publications.

[Type Inference In The Presence Of Subtyping: From Theory To Practice](https://hal.inria.fr/inria-00073205/document) describes how type inference can scale to a large codebase. In particular, handling subtyping and parametric polymorphism can make efficient type inference tricky, but this paper describes some practical techniques for making type inference work on real codebases.

[Componential Set-Based Analysis](https://users.soe.ucsc.edu/~cormac/papers/pldi97.pdf) describes how component-based analysis can help type inference scale to efficiently handle a large codebase. Reading this paper is useful to understand why Flow's implementation relies on breaking down code into a number of different modules which can be analyzed separately, which fortunately is the way most modern JavaScript is developed.

[The Ins And Outs Of Gradual Type Inference](http://dl.acm.org/citation.cfm?id=2103714) describes how unknown types can be worked into a type inference system. This research originally applied to ActionScript, but many of the principles are critical to Flow, because one of the key properties of Flow is that you can start typechecking on part of your codebase without converting the whole thing to be statically typed.

[Type Inference for Static Compilation of JavaScript](https://arxiv.org/abs/1608.07261) describes methods that specifically target JavaScript and extend past type inference and into an ahead-of-time compiler. If you're interested in the bleeding edge of type inference, this paper is for you.
