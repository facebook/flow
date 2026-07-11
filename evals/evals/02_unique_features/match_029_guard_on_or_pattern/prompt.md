Write a Flow function `shouldRetry(status: number, networkUp: boolean): boolean` that uses a `match` expression to decide whether a failed request should be retried.

A request is retried only when the network is up **and** the status is one of the gateway errors `502`, `503`, or `504`. Anything else is not retried.

Express the gateway-error check as a single case.
