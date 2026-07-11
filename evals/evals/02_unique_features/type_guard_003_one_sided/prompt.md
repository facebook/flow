`main.js` has a `format` function that turns an optional sensor reading into a
display string. It relies on a helper `isUsableReading` that has not been written
yet.

Implement `isUsableReading(temp: ?number)` so that `format` type-checks. A reading
is usable when it is present and falls within the supported operating range of
-40 to 125 degrees (inclusive); anything outside that range — including a missing
reading — is not usable.
