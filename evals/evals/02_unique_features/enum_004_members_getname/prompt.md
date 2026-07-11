Write Flow code for a log-level reporting system.

Define a number enum `LogLevel` with members: `Debug` (0), `Info` (1), `Warn` (2), `Error` (3), `Fatal` (4).

Write:
- `allLevelNames(): Array<string>` — use the enum's iteration method to collect the human-readable name of every member into an array, in declaration order
- `levelRange(min: LogLevel, max: LogLevel): Array<LogLevel>` — iterate over all members and return those whose numeric value is between `min` and `max` (inclusive). You will need to convert enum values to their underlying numbers to compare them.
- `formatLevel(level: LogLevel): string` — return a string like `"WARN (2)"` using the member's name (uppercased) and its numeric value
