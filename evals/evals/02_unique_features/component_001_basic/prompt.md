Write a Flow React component `UserCard` in `main.js`.

Props:
- `name: string` (required)
- `age: number` (required)
- `bio: string` (optional, defaults to "No bio provided")
- `isOnline: boolean` (optional, defaults to false)

The component should return a `<div>` containing:
- An `<h2>` with the name followed by the age category in parentheses:
  age < 13 is "Child", 13-19 is "Teen", 20-64 is "Adult", 65+ is "Senior"
- A `<p>` with "Age: {age}"
- A `<p>` with the bio, truncated to 100 characters with "..." appended if longer
- A `<span>` with either "Online" or "Offline" depending on the isOnline prop

The code must pass `flow check` with zero errors.
