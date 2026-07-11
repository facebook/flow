You are building a typed-id module split across two files: `Id.js` and `main.js`.

In `Id.js`:
- Define an ID type that is parameterized by the kind of entity it identifies, so that an ID for one entity kind cannot be passed where an ID for a different entity kind is expected — even though every ID is backed by a `string` internally. Other files must not be able to treat a `string` as an ID, or an ID as a `string`, except through the functions below.
- `makeId<TEntity>(raw: string)` — wrap a raw string as an ID for entity kind `TEntity`.
- `idToString<TEntity>(id)` — return the underlying string of an ID.

In `main.js`:
- Declare two object types: `User` (with a `name: string`) and `Post` (with a `title: string`).
- `userKey(id)` — take an ID for `User` and return `user:` followed by its underlying string.
- `postKey(id)` — take an ID for `Post` and return `post:` followed by its underlying string.
- `newUserId(raw: string)` — build an ID for `User` from a raw string.
