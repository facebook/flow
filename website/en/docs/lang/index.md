---
layout: guide
---

### JavaScript: The Good and the Bad <a class="toc" id="toc-javascript-the-good-and-the-bad" href="#toc-javascript-the-good-and-the-bad"></a>

Developers like coding in JavaScript because it helps them move fast. The
language facilitates fast prototyping of ideas via dynamic typing. The runtime
provides the means for fast iteration on those ideas via dynamic
compilation. This fuels a fast edit-refresh cycle, which promises an immersive
coding experience that is quite appealing to creative developers.

However, evolving and growing a JavaScript codebase is notoriously
challenging. Developers cannot move fast when they break stuff. They hit
frequent interruptions, spending a lot of time debugging silly mistakes,
unraveling assumptions and guarantees made by libraries written by others, etc.

In principle, this overhead can be mitigated by adding a layer of types to the
codebase, and building tools that use type information to solve the above
problems. For example, types can be used to identify bugs, to document
interfaces of libraries, and so on.

The idea of using types to manage code evolution and growth in JavaScript (and
related languages) is not new. In fact, several useful type systems have been
built for JavaScript in recent years. However, type systems differ in their
goals. On one end of the spectrum are permissive type systems that provide some
degree of linting against likely errors, without regard for correctness. On the
other end of the spectrum are restrictive type systems that can guarantee the
correctness of static code optimizations, at the cost of
interoperability. Another area that has not seen a lot of focus is the
performance of type checking itself.

## Flow: Goals <a class="toc" id="toc-flow-goals" href="#toc-flow-goals"></a>

Flow is a static type checker for JavaScript that we built at Facebook. The
overall mission of Flow is to deliver an immersive coding experience for
JavaScript developers---a fast edit-refresh cycle---even as the codebase
evolves and grows. In engineering terms, we identify two concrete goals that
are important to this mission: *precision* and *speed*. These goals pervasively
drive the design and implementation.

#### Precision <a class="toc" id="toc-precision" href="#toc-precision"></a>

JavaScript bugs can have significant impact at Facebook. Developers want to
find and fix as many bugs as they can by the time their code rolls out into
production. So we must care about soundness. At the same time, we must also
care about not reporting too many spurious errors, because a low signal/noise
ratio implies a low fix rate.

In other words, we want Flow's analysis to be precise in practice---it must
model essential characteristics of the language accurately enough to understand
the difference between idiomatic code and unintentional mistakes.

Precision also has other desirable consequences. When types are trustworthy,
developers tend to rely on them to structure their code and reason about it,
leading to cleaner and more efficient code with fewer dynamic checks. When type
errors are trustworthy, developers can focus on what their code does rather
than thinking about how to rewrite their code to satisfy (or work around) the
type system.

Finally, precision enables useful developer tools to be built. In particular,
the quality of results reported by Flow when the developer asks for the type of
an expression, the definition reaching a reference, or the set of possible
completions at a point through an IDE is correlated with the precision of
Flow's analysis.

#### Speed <a class="toc" id="toc-speed" href="#toc-speed"></a>

Precision usually comes at the cost of speed. But while a precise analysis is
desirable, most of the appeal of JavaScript is lost if we slow down the
edit-refresh cycle by making developers wait as we compile.

In other words, we must engineer Flow's analysis to be extremely fast---it must
respond to code changes without noticeable delay, while still being precise
enough in practice.

Like precision, speed also has other significant effects. When bugs are
reported as the developer makes changes to code, they become part of the
editing process---the developer doesn't need to run the code to detect bugs,
and tracing bugs back to the code becomes simpler. Similarly, when the IDE can
show the type of an expression, the definition reaching a reference, etc. as
the developer is coding, we have observed that productivity can improve
dramatically.

---

#### What makes Flow precise? <a class="toc" id="toc-what-makes-flow-precise" href="#toc-what-makes-flow-precise"></a>

One of the main contributors of Flow's precision is path-sensitivity: the way
types interact with runtime tests. The essence of many JavaScript idioms is to
put together ad hoc sets of runtime values and to take them apart with shallow,
structural (in)equality checks. In Flow, the set of runtime values that a
variable may contain is described by its type, and a runtime test on that
variable refines the type to a smaller set. This ability turns out to be quite
powerful and general in practice.

#### What makes Flow fast? <a class="toc" id="toc-what-makes-flow-fast" href="#toc-what-makes-flow-fast"></a>

The key to Flow's speed is modularity: the ability to break the analysis into
file-sized chunks that can be assembled later.

Fortunately, JavaScript is already written using files as modules, so we
modularize our analysis simply by asking that modules have explicitly typed
signatures. (We still infer types for the vast majority of code "local" to
modules.) Coincidentally, developers consider this good software engineering
practice anyway.

With modularity, we can aggressively parallelize our analysis. Furthermore,
when files change, we can incrementally analyze only those files that depend on
the changed files. Together, these choices have helped scale the analysis to
millions of lines of code.

Under the hood, Flow relies on a high-throughput low-latency systems
infrastructure that enables distribution of tasks among parallel workers, and
communication of results in parallel via shared memory. Combined with an
architecture where the analysis of a codebase is updated automatically in the
background on file system changes, Flow delivers near-instantaneous feedback as
the developer edits and rebases code, even in a large repository.
