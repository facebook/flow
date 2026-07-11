`main.js` defines an `Action` type: a disjoint union of counter operations (increment, decrement, reset, set).

Write a Flow function `processAction(count: number, action: Action): number` that returns the new count for the given action, using a `match` expression with object destructuring patterns.
