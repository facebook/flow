Write Flow code for a logging severity system.

Define an enum `Severity` with members `Info`, `Warning`, and `Error`.

Write:
- `formatSeverity(value: ?Severity): string` — the value may be missing. Return `"none"` when it is `null` or `undefined`. Otherwise return the member's name in lowercase (e.g. `"info"`). Do not hard-code the names — derive the label from the enum value itself.
- `severityRank(value: Severity | number): number` — the value is either a `Severity` or a raw numeric priority that some legacy callers still pass. If it is already a raw number, return it unchanged. Otherwise return the severity's position in declaration order (`Info`=0, `Warning`=1, `Error`=2). Don't hard-code each member's index — the ranks must stay correct on their own if a new severity is later inserted into the middle of the declaration.

Do not use a `switch` statement or a `match` expression in either function.
