---
id: declarations
title: Declarations
layout: docs
permalink: /docs/declarations.html
prev: classes.html
next: nullable-types.html
---

Take the following code snippet:

{% highlight javascript linenos=table %}
/* @flow */
var M = require('M');
M.foo(new C());
{% endhighlight %}

As is evident, the above code could be fraught with problems. Where is the `M` 
module? Where is `C` defined?

Running Flow on the above code yields:

```bbcode
/tmp/flow/f.js:2:9,20: M
Required module not found

/tmp/flow/f.js:3:11,11: identifier C
Unknown global name
```

## Declarations

Flow supports 
[declarations](third-party.html#example) 
via the `declare` keyword to allow the interface of code to be defined and 
then used in modules and other JavaScript code.

{% highlight javascript linenos=table %}
declare class C {
  x: string;
}
declare module M {
  declare function foo(c: C): void;
}
{% endhighlight %}

`C` and `M` are interfaces. The implementation details of `foo()`, for 
example, does does not need to be known by Flow. Just the types it exposes. 

The interfaces can be declared in any file you choose, as long as they can be seen by Flow.

To use code in declared files, tell the Flow server about it. Assume the code above is defined in `/lib/flow/`

```bbcode
flow start --lib /lib/flow/
```

Now Flow knows to check `/lib/flow/` for any code that is not immediately 
available in the file on which it is checking.

Assuming we have the declarations above, now when running Flow against this code:

{% highlight javascript linenos=table %}
var M = require('M');
M.foo(new C());
{% endhighlight %}

```bbcode
No errors!
```
