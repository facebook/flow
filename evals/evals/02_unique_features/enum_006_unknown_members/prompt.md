Write Flow code for handling API response codes that may evolve over time. The enum may gain new members in the future, so the code must handle unknown values gracefully.

Define a string enum `ApiCode` with members `Success`, `RateLimit`, `Maintenance`, plus unknown members (the enum can have additional members added later that existing code hasn't seen).

Write:
- `handleResponse(code: ApiCode): string` — return `"proceed"` for `Success`, `"backoff"` for `RateLimit`, `"retry later"` for `Maintenance`. For any unknown future member, return `"unknown response"`.
- `isSafeToRetry(code: ApiCode): boolean` — return `true` for `RateLimit` and `Maintenance`, `false` for `Success`, and `false` for any unknown member
