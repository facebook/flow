---
id: declarations
title: Declarations
layout: docs
permalink: /docs/declarations.html
prev: primitives.html
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

The declarations for `C` and `M` have global scope. This means that the name `C` can be referenced
directly in any file, and the module `M` can be required in any file.

Declarations can be thought of as interfaces.
The implementation details of `foo()`, for 
example, does does not need to be known by Flow: just the types it exposes is enough. 

> NOTE
>
> The interfaces can be declared in any file you choose, as long as they are in a directory 
outside the paths monitored by `.flowconfig`.

To use code in declared files, tell the Flow server about it. Assume the code above is defined in `/lib/flow/`

```bbcode
flow start --lib /lib/flow/
```

It is also possible to specify multiple library paths, separated by comma.
Furthermore, you can specify the libs in `.flowconfig` with the `[libs]` option.

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

## Paths

A module declaration can also use a string literal to describe the path used to refer to that module in `require` statements.
Thus, when you have:
{% highlight javascript linenos=table %}
var M = require('path/to/M');
...
{% endhighlight %}

Use a declaration of the form:
{% highlight javascript linenos=table %}
declare module "path/to/M" {
  ...
}
{% endhighlight %}
