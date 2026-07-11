Write a Flow hook `useMap` in `main.js` that manages a key-value map as React state.

The hook is generic over the key and value types. It takes an optional initial list of key-value pairs (as a read-only array of tuples).

Return an object with these operations:
- `get(key)`: returns the value for a key, or `undefined` if not found
- `set(key, value)`: adds or updates an entry
- `delete(key)`: removes an entry
- `has(key)`: returns whether a key exists
- `size`: the current number of entries
- `entries`: a read-only array of all `[key, value]` pairs

Use `useState` with a `Map` internally. Each mutating operation should create a new `Map` to trigger re-renders.

The code must pass `flow check` with zero errors.
