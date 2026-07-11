You are building an email module split across two files: `Email.js` and `main.js`.

In `Email.js`:
- Define a type for validated email addresses. It is backed by a string internally, but other files must not be able to treat an arbitrary string as a validated email, nor read a validated email back as a plain string — the only way to obtain one is through the functions below.
- `parseEmail(raw: string)` — return a validated email if `raw` is a well-formed address (some non-empty text, then an `@`, then a domain containing a dot), otherwise `null`.
- `domainOf(email)` — return the part of the address after the `@`.

In `main.js`:
- `isCorporate(email)` — return whether the email's domain is exactly `meta.com`.
- `collectValid(raws: ReadonlyArray<string>)` — given raw strings, return an array containing the validated emails for the ones that are well-formed.
