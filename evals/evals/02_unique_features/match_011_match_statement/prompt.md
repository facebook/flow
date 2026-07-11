Write a Flow function `execute(store: Map<string, string>, command: Command): void` that uses a `match` statement to perform side effects on the store.

- `'log'`: call `console.log` with the message
- `'set'`: call `store.set(key, value)`
- `'delete'`: call `store.delete(key)`
