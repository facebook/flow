---
id: react-example
title: A Flux & React demo
layout: docs
permalink: /docs/react-example.html
prev: advanced-configuration.html
next: underscore.html
---

For those familiar with the JavaScript library
[React](http://facebook.github.io/react/index.html), Flow fits quite nicely.
In this case study, we take an existing React project and attempt to have
Flow be able to successfully type check its code. While it will not be an
automatic, run the Flow type-checker only one time win, we will show that it
does not need to be painful to use Flow on your existing projects.

## Getting Started - Download Flux Chat

In order to follow the steps below, please 
[download Flux Chat](https://github.com/facebook/flux/tree/a9724ae9dedd25daa5f0127ee54343353c5cbfd6/examples/flux-chat)
. Flux Chat is a canonical example for the
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

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/d2c099065ac58fb78b5f3951d7ac912de5e5a58c) 
> of the changes made in this section. 
> 

### Flow Server

After all files have been annotated with `@flow`, we are ready to start our
Flow server which watches over the now Flow-aware JavaScript files and any
changes to them. From the root directory (i.e., the one with the
`.flowconfig` file)

```bash
$> flow start --lib lib
```

The `--lib` specifies that any [declarations](declarations.html) that are 
needed will be placed in that directory.

### Type Checking

Now the moment of first truth arrives. You can call `flow` to query the Flow server for any type-checking errors ... and [there will be many](https://gist.github.com/JoelMarcey/1629e39f3afc789b0f0e).

```bash
$> flow
Found 50 errors
```

> NOTE
>
> Throughout this walk-through, your error list may be slightly
> different than shown here.

## Working Around Current Flow Limitations

### `object-assign`

At this stage we have already hit some limitations of Flow. Flow does not know 
about the module `object-assign`. However, it understands what `Object.assign` does. Thus, we will replace any `object-assign` usage by `Object.assign`. 

However, since `Object.assign` may not be implemented by all JavaScript engines, we still need to "polyfill" it. We will do so by putting it in a file outside of Flow's knowledge (i.e., in a file *without* `/* @flow */`) like this:

{% highlight javascript linenos=table %}
if (!Object.assign) {
  Object.assign = require('object-assign');
}
{% endhighlight %}

### Class Inheritance

Flow does not allow class instances to be extended at runtime. Thus, the following code will not work:

{% highlight javascript linenos=table %}
var ChatAppDispatcher = Object.assign(new Dispatcher(), /*...*/);
{% endhighlight %}

Instead, we need to work around this by exporting the `Dispatcher` interface 
by hand. This results in somewhat ugly code, but is the only way to get around explicit class inheritance for now. This is what it roughly looks like:

{% highlight javascript linenos=table %}
var _dispatcherInstance = new Dispatcher();

var ChatAppDispatcher = {
  register: _dispatcherInstance.register.bind(_dispatcherInstance),
  /*...*/
};
{% endhighlight %}

> IMPORTANT: 
> 
> Currently there is a bug in Flow which makes it think that Object.assign 
> copies all properties visible on an object, and not just own properties. 
> Thus, Flow thinks that the following would extend a `Dispatcher` instance, 
> even though it doesn't:
> 
> `var ChatAppDispatcher = Object.assign({}, new Dispatcher(), /*...*/);`

### Module Files

Finally regarding Flow limitations, we currently cannot create interface 
declaration files for the modules `react/lib/keyMirror` and `react/lib/cx`. 
This results in missing module errors that we need to work around. As a 
result, we disabled Flow for the files importing these two modules. While we disabled flow in these files, we did annotate and perform other Flow-aware changes so we can be error free once we do enable Flow. 

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/4a4f3aaf512eb619e5f8d9a82432b61b75710927) 
> of the changes for this section and here is the updated 
> [error output](https://gist.github.com/JoelMarcey/76d1d5aaf2717ffb32f0).


```bash
$> flow
Found 29 errors
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

### General Annotation Strategy

Our first pass is to basically start annotating. We will skip those that
we are unable to annotate.

Modules such as `MessageStore.js` had functions that returned a message given 
a string id.

{% highlight javascript linenos=table %}
get: function(id) {
  return _messages[id];
},
{% endhighlight %}

What type is `id`? What exactly is a `message`? Flow helps us create higher quality code.

{% highlight javascript linenos=table %}
type Message = {
  id: string;
  threadID: string;
  authorName: string;
  date: Date;
  text: string;
  isRead: boolean;
};
// ...
get: function(id: string): ?Message {
  return _messages[id];
},
{% endhighlight %}

After this first pass through the modules, we will go through the
modules again, taking into account the type information we learned from other
modules. Rinse and repeat this pass until we get a fully annotated interface
for all modules.

### Using Instances of Polymorphic Types

Instances of polymorphic types can be used with arbitrary types by client 
code. Thus, if one wants to force a specific type, one needs to manually 
specify it for an instance. An example, where we need to do this is in `js/
dispatcher/ChatAppDispatcher.js`. This file exports an extended instance of 
Flux's `Dispatcher` class. We type `Dispatcher` as a polymorphic class and 
only want to allow values of specific types to be passed through it. To limit 
the payloads the dispatcher accepts we type the instance explicitly:

{% highlight javascript linenos=table %}
var _dispatcherInstance: Dispatcher<PayloadType> = new Dispatcher();

var ChatAppDispatcher = {
// [...]
{% endhighlight %}

> NOTE
>
> It is currently not possible to use syntax like 
> `new Dispatcher<PayloadType>()`.

### Narrowing Union Types

We typed `ChatAppDispatcher`'s actual payload data as a big union over different types of data. The listeners then need to decide what type the data actually is. For this, the following pattern is used:

{% highlight javascript linenos=table %}
switch(action.type) {
case ActionTypes.CLICK_THREAD:
  // Handle action as ViewClickThreadAction
  break;

case ActionTypes.RECEIVE_RAW_MESSAGES:
  // Handle action as ServerReceiveRawMessagesAction
  break;

// [...]
}
{% endhighlight %}

Ideally, we would want to narrow down the type of action inside the individual cases. However, this is currently not possible with Flow. Not all hope is lost though. Flow supports `any` as type. 

> WARNING
> 
> Try to limit `any` use to places you hit limitations of Flow. In the latter 
> case, check whether the issue is known. If it is not, we ask you to report 
> it to us. Then, we can decide if and how we can improve Flow.

This does not give us type checking guarantees on the use of action. However, it will allow you to still take advantage of Flow in the rest of your code base.

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/c85f06b9f555cf3f95fc4d9ef0b8f683260fec40) 
> of the changes for this section and here is the updated 
> [error output](https://gist.github.com/JoelMarcey/266d8696fa30268c2d04).


```bash
$> flow
Found 17 errors
```

## Tests

If you now look at the current
[list of errors](https://gist.github.com/JoelMarcey/a41d15d5b8c72b73c23a)
, you will notice some relating to our test file `UnreadThreadStore-test.js`.
For now, we are going to only weakly check the test file using `@flow-weak`.

{% highlight javascript linenos=table %}
/**
 * @flow weak
 */
{% endhighlight %}

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/8325417064a21bd9cd819bfd0e00466df6387594) 
> of the changes for this section and here is the updated 
> [error output](https://gist.github.com/JoelMarcey/81813064b07fe6892cc1).


```bash
$> flow
Found 8 errors
```


## The Real Work

Until this point, we have been postponing and mitigating the errors discovered
by the Flow type-checker. Now we are at the point where we can see the real
benefits of Flow as it shows real problems with our project code.

We start with the low-hanging fruit and move our ways upwards until we have
no Flow errors left in our code.

### Missing Parameters

{% highlight javascript linenos=table %}
ChatWebAPIUtils.createMessage(message);
// ...
createMessage: function(message: Message, optThreadName: string) {
{% endhighlight %}

```bbcode
js/actions/ChatMessageActionCreators.js:30:5,42: call of method createMessage
Too few arguments
  js/utils/ChatWebAPIUtils.js:40:18,61:3: function
```

In vanilla JavaScript, missing parameters are undefined. This can lead to
unexpected behavior or crashes. Flow will not allow code to skip parameters
unless they are specifically marked as optional with `?`. In this case, we
have one instance that requires optional.


{% highlight javascript linenos=table %}
createMessage: function(message: Message, optThreadName?: string) {
  var threadName = optThreadName || 'New Conversation';
{% endhighlight %}

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/62cef6dd10be00bbb96f9cdd8ae4fb0a059e6014) 
> of the changes for this section and here is the updated 
> [error output](https://gist.github.com/JoelMarcey/bd2cd831f8a593b66ddf).


```bash
$> flow
Found 7 errors
```

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
types. 

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/22eb39e8bc48a598ae5518db564deb1959ed6740) 
> of the changes for this section] and here is the updated 
> [error output](https://gist.github.com/JoelMarcey/591a2e4d1e7942d524c3).


```bash
$> flow
Found 4 errors
```

## Other Errors

Now let's fix the other errors to make Flow happy with no errors.

### Property Access

In `ThreadsStore.js`,

{% highlight javascript linenos=table %}
if (thread && thread.lastTimestamp > message.timestamp) {
// ...
{% endhighlight %}

```bbcode
js/stores/ThreadStore.js:62:21,40: property lastTimestamp
Property lastTimestamp not found in object:
  js/stores/ThreadStore.js:38:15,42:1: object type
```

We have code that tries to access a property which does not exist in a given
object. We fix this by using an existing property instead.

{% highlight javascript linenos=table %}
if (thread && thread.lastMessage.date.getTime() > message.timestamp) {
// ...
{% endhighlight %}

### Null Variable

In `MessageSection.js`,

{% highlight javascript linenos=table %}
 <h3 className="message-thread-heading">{this.state.thread.name}</h3>
// ...
{% endhighlight %}

```bbcode
js/components/MessageSection.react.js:78:49,70: property name
This type is incompatible with
  js/components/MessageSection.react.js:38:12,17: ?Thread
```

Here we try to access a variable that could possibly be `null`. In this case,
you check for the variable being `null` and add special handling for that case.

{% highlight javascript linenos=table %}
render: function(): any {
  var thread = this.state.thread;
  var name = thread ? thread.name : "";
  var messageListItems = this.state.messages.map(getMessageListItem);
  return (
    <div className="message-section">
    <h3 className="message-thread-heading">{name}</h3>
// ...
{% endhighlight %}

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

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/564b2dd26d71c7cd300052fb831e0104e24b5292) 
> of the changes for this section.


```bash
$> flow
Found 4 errors
```


## No Errors!

If all went according to plan,
[this](https://gist.github.com/JoelMarcey/30145418434c0b0a25ea)
is what you should see when running `flow` for the final time.

```bash
$> flow
No errors!
```

## Stripping Annotations

Finally, we need to adapt the building process to strip type annotations.
For this we need to update the `reactify` dependency in `package.json` to 
0.17.0. Starting from this version `reactify` allows to strip type 
annotations. The changes needed are trivial. We only need to set the 
`stripTypes` to `true`. Afterwards, we do the following to start the automated 
building process:

```bash
npm install
npm start
```

Now we can start a http server and check our results in the browser:

```bash
python -m SimpleHTTPServer
```

and then navigate to `localhost:8000`.

> CODE CHECK
> 
> Here is a [diff](https://github.com/facebook/flow/commit/8e19ceca38e96cff3b372347e1c356c09fd02b7c) 
> of the changes for this section.
