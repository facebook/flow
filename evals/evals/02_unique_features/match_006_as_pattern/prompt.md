`main.js` defines a `Response` type: the union of an ok response (with a nested `data`), an error response (with a `code` and `message`), and a loading state.

Write Flow code that uses `match` to summarize a response, passing refined types to helper functions. Write helper functions that format ok and error responses, and a `summarize(response: Response): string` function that uses match to dispatch to them. For `'loading'`, return `"Loading..."`.
