Write Flow code that operates on Flow Enums generically.

Define two string enums for testing:
- `Color` with members `Red`, `Green`, `Blue`
- `Size` with members `Small`, `Medium`, `Large`

Then write generic utility functions that work with **any** string-backed Flow Enum (not just `Color` or `Size`). The functions should accept the enum object itself as a parameter so callers can pass in any enum.

- `enumToArray` — accept any string enum and return an `Array<{label: string, value: string}>` by iterating over all members. `label` is the member's name and `value` is the underlying string.
- `safeParseEnum` — accept any string enum and a raw `string`. Try to convert the string to a valid enum value. Return the enum value if valid, or `null` if not.
- `enumIncludes` — accept any string enum and a raw `string`. Return `true` if the string corresponds to a valid member, `false` otherwise.
