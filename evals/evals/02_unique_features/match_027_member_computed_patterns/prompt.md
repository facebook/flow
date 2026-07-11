A constants object `HttpStatus` mapping names to HTTP status codes is provided.

Write a Flow function `retryDelayMs(status: RetryableStatus, attempt: number): number` that uses a `match` expression to compute how many milliseconds to wait before retrying a failed request. Refer to each status through the `HttpStatus` object in your patterns rather than hard-coding the numeric codes, so the policy keeps working if the codes are ever edited in one place.

- `tooManyRequests`: wait `1000 * attempt` ms (linear backoff)
- `serverError` and `badGateway`: wait `250 * 2 ** attempt` ms (exponential backoff)
- `gatewayTimeout`: wait `500 * 2 ** attempt` ms
