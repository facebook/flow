Write a Flow function `cachePolicy(entry: CacheEntry): string` that uses a `match` expression to describe how a response should be cached.

The `CacheEntry` type is provided. Each entry has a `'content-type'` (`'json'` or `'html'`) and a `'max-age'` in seconds. Return:

- `"cache json for <max-age>s"` for JSON responses
- `"cache html for <max-age>s"` for HTML responses
