Write three custom React hooks in `main.js` using Flow.

`useToggle` takes an initial boolean value and returns a tuple of the current value and a toggle function that flips it.

`useCounter` takes an initial count and an optional step size (defaults to 1). It returns an object with `count`, `increment`, `decrement`, and `reset` (which restores the initial value).

`useDebounce` takes a string value and a delay in milliseconds. It returns the debounced value — the input value is only reflected after the specified delay passes with no further changes. Use `useEffect` with a `setTimeout`/`clearTimeout` pattern internally.

The code must pass `flow check` with zero errors.
