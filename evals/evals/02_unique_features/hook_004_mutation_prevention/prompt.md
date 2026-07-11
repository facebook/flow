The code in `main.js` has type errors. Fix it so it passes `flow check` with zero errors.

Keep the same behavior: `setCategory` replaces the current category, and `addTag` appends a tag to the existing list. The hook must update its state by producing a new state value on each change instead of modifying the current one in place.
