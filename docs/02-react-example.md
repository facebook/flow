---
id: react-example
title: A Flux & React demo
layout: docs
permalink: /docs/react-example.html
prev: troubleshooting.html
next: underscore.html
---

For those familiar with the JavaScript library
[React](http://facebook.github.io/react/index.html), Flow fits quite nicely.
In this case study, we take an existing React project and attempt to have
Flow be able to successfully type check its code. While it will not be an
automatic, run the Flow type-checker only one time win, we will show that it
does not need to be painful to use Flow on your existing projects.

## The React Source

We will be using
[Flux Chat](https://github.com/facebook/flux/tree/a9724ae9dedd25daa5f0127ee54343353c5cbfd6/examples/flux-chat)
as our guinea pig. Flux Chat is a canonical example for the
[Flux](http://facebook.github.io/flux/docs/overview.html)
application architecture framework built on React.

## Preparation

Flux Chat is split over
[multiple module files](https://github.com/facebook/flux/tree/a9724ae9dedd25daa5f0127ee54343353c5cbfd6/examples/flux-chat/js).

### `.flowconfig`

In order for Flow to be able to begin to type check these files, we must
create a `.flowconfig` file in the root directory of those files. This file
signals to the Flow type-checker where to begin type checking your code.

```bash
$> flow init
```

### `@flow`

Next, each JavaScript implementation file in the root and below must be
[annotated with `@flow` inside the first comment block of the file](https://github.com/facebook/flow/commit/d2c099065ac58fb78b5f3951d7ac912de5e5a58c)
. For example, in
[`js/components/ChatApp.react.js`](https://github.com/facebook/flow/blob/d2c099065ac58fb78b5f3951d7ac912de5e5a58c/js/components/ChatApp.react.js):

{% highlight javascript linenos=table %}
/**
 * This file is provided by Facebook for testing and evaluation purposes
 * only. Facebook reserves all rights not expressly granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * FACEBOOK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * @flow
 */

var MessageSection = require('./MessageSection.react');
{% endhighlight %}

### Flow Server

After all files have been annotated with `@flow`, we are ready to start our
Flow server which watches over the now Flow-aware JavaScript files and any
changes to them. From the root directory (i.e., the one with the
`.flowconfig` file)

```bash
$> flow start
```

### Type Checking

Now the moment of first truth arrives. You can call `flow` to query the Flow server for any type-checking errors ... and [there will be many](https://gist.github.com/JoelMarcey/1629e39f3afc789b0f0e).

```bash
$> flow
Found 50 errors
```

> NOTE
>
> throughout this walk-through, your error list may be slightly
> different than shown here.

Many of these errors are related to
[missing NodeJS modules](https://github.com/facebook/flow/commit/f618cde8b0d9324819faf381004ca6f1fc93bab1)
.

> NOTE
>
> For simplicity, we just stubbed out the necessary module files. Obviously,
> you will want to use real module code.

Also, the [`object-assign` module must be replaced by direct calls to
`Object.assign`](https://github.com/facebook/flow/commit/5da7bf10a3ce3493a03da0a516e36d1de93fc920) in order to make Flow work correctly with this usage pattern.

As you will see, the
[list of errors](https://gist.github.com/JoelMarcey/8817aff7637cec1024a3)
has been reduced.

```bash
$> flow
Found 32 errors
```


## Annotating Module Exports

Flow requires the annotation of objects that a module exports. Forcing these
annotations allows flow to more easily check that imported modules are being
used as intended. Additionally, and possibly more importantly, is that these
annotations make both using modules and reading module code much easier.

### Flux-Chat Modules

Now we will annotate all the modules that exist in Flux-Chat. We will ignore
cross-module and module internal errors for now. While this may seem
counter-intuitive, doing this will allow us to get a better picture of the
actual errors hidden in our project (as we will soon see).

Our first pass is to basically start annotating. We will skip those that
we are unable to annotate. After this first pass, we will go through the
modules again, taking into account the type information we learned from other
modules. Rinse and repeat this pass until we get a [fully annotated interface
for all modules](https://github.com/facebook/flow/commit/c4ed17c637da554e1975198920d971fc17fd10f0).

### Tests

If you now look at the current
[list of errors](https://gist.github.com/JoelMarcey/a41d15d5b8c72b73c23a)
, you will notice some relating to our test file `UnreadThreadStore-test.js`.
For now, we are going to only
[weakly check the test file using `@flow-weak`](https://github.com/facebook/flow/commit/297d371662106d118b5798cc013f1da2cf8aec3d).

{% highlight javascript linenos=table %}
/**
 * @flow weak
 */
{% endhighlight %}

After this module annotation step, the
[list of errors](https://gist.github.com/JoelMarcey/96b64f42860dd60227fc)
will be much more manageable, and, more importantly, will be the errors we
actually care about with respect to types in our React project.

```bash
$> flow
Found 11 errors
```


## The Real Work

Until this point, we have been postponing and mitigating the errors discovered
by the Flow type-checker. Now we are at the point where we can see the real
benefits of Flow as it shows real problems with our project code.

We start with the low-hanging fruit and move our ways upwards until we have
no Flow errors left in our code.

### Preventing Calls to a Missing Method

> NOTE
>
> This error may not currently be in your list of errors do to a fix that
> has already been applied.

Take the following error:

```bbcode
js/components/MessageSection.react.js:70:5,53: call of method removeChangeListener
Property removeChangeListener not found in object:
  js/stores/MessageStore.js:60:34,35: object literal
```

This is one of those errors that will cause us pain at runtime. Since Flow finds this error before our code runs, we can be reduce this pain point.

The fix is to copy the `removeChangeListener` implementation of one
of the two other store implementations.

{% highlight javascript linenos=table %}
removeChangeListener: function(callback) {
  this.removeListener(CHANGE_EVENT, callback);
},
{% endhighlight %}

Interestingly enough, this error was actually [caught before the release
of Flow](https://github.com/facebook/flux/commit/f828ecae10cdf15ed1ba2fd1210dcf671365bbe4)
; however, with Flow, this mistake need not be made again as it will be
caught during development time.

### Missing Parameters

```bbcode
js/actions/ChatMessageActionCreators.js:30:5,42: call of method createMessage
Too few arguments
  js/utils/ChatWebAPIUtils.js:40:18,61:3: function
```

In vanilla JavaScript, missing parameters are undefined. This can lead to
unexpected behavior or crashes. Flow will not allow code to skip parameters
unless they are specifically marked as optional with `?`. In this case, we
have
[one instance that requires optional](https://github.com/facebook/flow/commit/3a86ad46132907ab2f5320a5e162f2e1fcfcb9a8).


{% highlight javascript linenos=table %}
createMessage: function(message: Message, optThreadName?: string) {
  var threadName = optThreadName || 'New Conversation';
{% endhighlight %}


### Property Use

Flow can also catch some React specific errors that may have been introduced.
Take the `MessageListItem` component, for example. Flow discovers the
following errors.

```bbcode
js/components/MessageListItem.react.js:29:46,63: property authorName
This type is incompatible with
  js/components/MessageListItem.react.js:22:14,34: ?any

js/components/MessageListItem.react.js:31:12,23: property date
This type is incompatible with
  js/components/MessageListItem.react.js:22:14,34: ?any

js/components/MessageListItem.react.js:33:40,51: property text
This type is incompatible with
  js/components/MessageListItem.react.js:22:14,34: ?any
```

As the
[source of `MessageListItem`](https://github.com/facebook/flow/blob/flux-chat-example/js/components/MessageListItem.react.js)
shows, the `message` property is annotated through
[React's `propType` feature](http://facebook.github.io/react/docs/reusable-components.html)
. However, it is specified as `message: ReactPropTypes.object`, which, in React
means that message is an **optional** property. Thus, the property might not
exist in all instances of the component. This can lead to the consequence of
undefined access. This can be seen in the Flow error output from the
question mark in `?any`. Luckily, React also allows us to specify that a
property is required and, of course, Flow understands this.

{% highlight javascript linenos=table %}
message: ReactPropTypes.shape({
  authorName: ReactPropTypes.string.isRequired,
  date: ReactPropTypes.instanceOf(Date).isRequired,
  text: ReactPropTypes.string.isRequired
}).isRequired
{% endhighlight %}

If you are familiar with React, you might be under the impression that React
will be able to check this too. However, React does this at *runtime*, and
only in development mode. Flow will catch these errors at *development
time*.  As a result, if some client would do something like
`<MessageListItem />`, Flow will discover the missing message property when checking your source. By using `shape` flow will also assure that all
properties of `message` are actually specified. Thankfully, our example
code always passed a correct message property.

We also adapt other components' `propTypes` to property reflect the expected
types. All the changes for this step are found
[here](https://github.com/facebook/flow/commit/a9208f94abb48d6300a4cc848e66ea4edfaa2816).

## Other Errors

Now let's fix the
[other errors](https://gist.github.com/JoelMarcey/2e039d8062d5ccb65cec)
to make Flow happy with no errors.

### Property Access

```bbcode
js/stores/ThreadStore.js:62:21,40: property lastTimestamp
Property lastTimestamp not found in object:
  js/stores/ThreadStore.js:38:15,42:1: object type
```

We have code that tries to access a property which does not exist in a given
object. We fix this by using an existing property instead.

### Null Variable

```bbcode
js/components/MessageSection.react.js:78:49,70: property name
This type is incompatible with
  js/components/MessageSection.react.js:38:12,17: ?Thread
```

Here we try to access a variable that could possibly be `null`. In this case,
you check for the variable being `null` and add special handling for that case.

### The Final Two Errors

> NOTE
>
> These errors may not be in your list of errors depending on how you
> annotated the code above, but just in case, this is how these type of
> errors can be resolved.

Here are the final two errors:

```bbcode
js/stores/MessageStore.js:38:14,19: ?string
This type is incompatible with
  js/stores/MessageStore.js:23:13,18: string

js/stores/ThreadStore.js:126:30,35: ?string
This type is incompatible with
  js/stores/MessageStore.js:23:13,18: string
```

These errors come from the following piece of code:

{% highlight javascript linenos=table %}
var message = MessageStore.getCreatedMessageData(action.text);
_messages[message.id] = message;
{% endhighlight %}

Here two different types come together. Investigation of this error shows
that it results from a bigger design issue in the implementation of flux-chat.
However, there is no way to actually make the code produce `null` at runtime
right now. Thus, we will take an escape hatch and make Flow not complain about
it anymore. We do this by simply annotating message with the type `any`. Flow
will then assume `message` can be assigned to any type.

The whole changes for this part of the walk-through can be found
[here](https://github.com/facebook/flow/commit/1ba269c2f5ec53e34f0d61800feb217a86e839f1)
.

## No Errors!

If all went according to plan,
[this](https://gist.github.com/JoelMarcey/30145418434c0b0a25ea)
is what you should see when running `flow` for the final time.

```bash
$> flow
No errors!
```
