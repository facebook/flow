---
layout: guide
---

## Switch Statement Exhaustiveness

Flow currently does not check for exhaustiveness in switch statements. So this,
gives you a type error for now.

```js
function flip(input: boolean): boolean {
  switch(input) {
    case true:
      return false;
    case false:
      return true;
  }
}
```

You need to add a default case for flow to be sure about your return type.

However, you can still use Flow to check if you check for all cases in a switch
statement. You

A common use case is Redux. You often want to make sure that you accounted for
all your action types in your reducer.

```js

declare var exhaustive: (action: empty) => any;

type State = number;
type Action =
  | {type: 'INCREMENT' }
  | {type: 'DECREMENT' }
  | {type: 'DOUBLE'}
  ;

function reducer(state: State, action: Action): State {
  switch(action.type) {
    case 'INCREMENT':
      return state + 1;
    case 'DECREMENT':
      return state - 1;
    default:
      /*::
      // $ExpectError -- {type: 'DOUBLE'} not handled
      exhaustive(action);
      */
      return state;
  }
}
```

Here, since you forgot to handle the 'DOUBLE' action, flow will complain that
the function `exhaustive` was called with a non-empty action type.

However, if you handle all the action types, flow will know that `exhaustive`
will never be called.
