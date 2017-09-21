---
layout: guide
---

[Redux](http://redux.js.org) has three major parts that should be typed:

- State
- Actions
- Reducers

### Typing Redux state <a class="toc" id="toc-typing-redux-state" href="#toc-typing-redux-state"></a>

Typing your [state](http://redux.js.org/docs/introduction/ThreePrinciples.html#single-source-of-truth)
object, works the same as typing any other object in Flow.

```js
type State = {
  users: Array<{
    id: string
    name: string,
    age: number,
    phoneNumber: string,
  }>,
  activeUserID: string,
  // ...
};
```

We can use this type alias to make sure reducers work correctly.

##### Typing Redux state immutability <a class="toc" id="toc-typing-redux-state-immutability" href="#toc-typing-redux-state-immutability"></a>

Redux state [is meant to be immutable](http://redux.js.org/docs/introduction/ThreePrinciples.html#state-is-read-only):
creating a new state object instead of changing properties on a single object.

You can enforce this in Flow by making every property effectively "read-only"
using "covariant" properties throughout your state object.

```js
type State = {
  +users: Array<{
    +id: string
    +name: string,
    +age: number,
    +phoneNumber: string,
  }>,
  +activeUserID: string,
  // ...
};
```

Now Flow will complain when you try to write to any of these properties.

```js
// @flow
type State = {
  +foo: string
};

let state: State = {
  foo: "foo"
};

state.foo = "bar"; // Error!
```

### Typing Redux actions <a class="toc" id="toc-typing-redux-actions" href="#toc-typing-redux-actions"></a>

The base type for Redux [actions](http://redux.js.org/docs/basics/Actions.html)
is an object with a `type` property.

```js
type Action = {
  +type: string,
};
```

But you'll want to use more specific types for your actions using disjoint
unions and each individual type of action.

```js
type Action =
  | { type: "FOO", foo: number }
  | { type: "BAR", bar: boolean }
  | { type: "BAZ", baz: string };
```

Using disjoint unions, Flow will be able to understand your reducers much
better.

##### Typing Redux action creators <a class="toc" id="toc-typing-redux-action-creators" href="#toc-typing-redux-action-creators"></a>

In order to type your Redux [action creators](http://redux.js.org/docs/basics/Actions.html#action-creators),
you'll want to split up your `Action` disjoint union into separate action
types.

```js
type FooAction = { type: "FOO", foo: number };
type BarAction = { type: "BAR", bar: boolean };

type Action =
  | FooAction
  | BarAction;
```

Then to type the action creator, just add a return type of the appropriate
action.

```js
// @flow
type FooAction = { type: "FOO", foo: number };
type BarAction = { type: "BAR", bar: boolean };

type Action =
  | FooAction
  | BarAction;

function foo(value: number): FooAction {
  return { type: "FOO", foo: value };
}

function bar(value: boolean): BarAction {
  return { type: "BAR", bar: value };
}
```

##### Typing Redux thunk actions <a class="toc" id="toc-typing-redux-thunk-actions" href="#toc-typing-redux-thunk-actions"></a>

In order to type your Redux [thunk actions](http://redux.js.org/docs/advanced/AsyncActions.html#async-action-creators),
you'll add types for `ThunkAction` as a function `Dispatch`, and `GetState`. `GetState` is a function that returns `State`.
`Dispatch` is an intersection of several dispatch flavors: the plain version, the version that works for thunks,
and the version that works for Promises. Of course you can define only the ones you need.

```js
import type { Store } from 'redux';

// plain redux
type PlainDispatch = (action: Action) => Action;

// redux-thunk
type ThunkDispatch = <R>(action: ThunkAction<R>) => R;
type ThunkAction<R> = (dispatch: Dispatch, GetState) => R;
type GetState = () => State;

// redux-promise-middleware
type PromiseDispatch = <R>(action: PromiseAction<R>) => Promise<{ value: R, action: Action }>;
type PromiseAction<R> = { payload: Promise<R> } | { payload: { promise: Promise<R> }};

// Intersection of all dispatch methods
type Dispatch = PlainDispatch & ThunkDispatch & PromiseDispatch;

// This is the type of our Redux Store
type AppStore = Store<State, Action, Dispatch>;
```

Then to type a thunk action creator, add a return type of a `ThunkAction` to your action creator
with the appropriate specialization.

```js
function foo(): ThunkAction<string> {
  return (dispatch, getState) => {
    const baz = getState().baz;
    dispatch({ type: "BAR", bar: true });
    return doSomethingAsync(baz)
      .then(value => {
        dispatch({ type: "FOO", foo: value });
        return value;
      });
    }
}
```

### Typing Redux reducers <a class="toc" id="toc-typing-redux-reducers" href="#toc-typing-redux-reducers"></a>

[Reducers](http://redux.js.org/docs/basics/Reducers.html) take the state and
actions that we've typed and pulls them together for one method.

```js
function reducer(state: State, action: Action): State {
  // ...
}
```

You can also validate that you have handled every single type of action by
using the `empty` type in your `default` case.

```js
// @flow
type State = { +value: boolean };

type FooAction = { type: "FOO", foo: boolean };
type BarAction = { type: "BAR", bar: boolean };

type Action = FooAction | BarAction;

function reducer(state: State, action: Action): State {
  switch (action.type) {
    case "FOO": return { ...state, value: action.foo };
    case "BAR": return { ...state, value: action.bar };
    default:
      (action: empty);
      return state;
  }
}
```

### Flow + Redux resources <a class="toc" id="toc-flow-redux-resources" href="#toc-flow-redux-resources"></a>

- [Using Redux with Flow](http://frantic.im/using-redux-with-flow) - Alex Kotliarskyi
- [Redux and Flowtype](https://medium.com/@cdebotton/redux-and-flowtype-69ff1dd09036#.fsrm1amlk) - Christian de Botton
