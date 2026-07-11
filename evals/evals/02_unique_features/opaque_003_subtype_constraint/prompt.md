You are building a user-id module split across two files: `UserId.js` and `main.js`.

In `UserId.js`:
- Define a type for user IDs. A user ID is backed by a string, and other files should be able to read a user ID anywhere a `string` is accepted — building URLs, sorting, logging. But other files must not be able to turn an arbitrary `string` into a user ID; that can only happen through the function below.
- `toUserId(raw: string)` — return a user ID if `raw` is all digits, otherwise `null`.

In `main.js`:
- `buildProfileUrl(id: UserId)` — return `https://internal/profile/` followed by the id.
- `sortIds(ids: ReadonlyArray<UserId>)` — return the ids sorted in ascending alphabetical order.
