Write Flow code for a build pipeline using enums and match expressions.

Define a string enum `BuildStep` with members `Lint`, `Compile`, `Test`, `Package`, `Deploy`.

Define a number enum `ExitCode` with members `Success` (0), `Warning` (1), `Failure` (2).

Write:
- `stepDuration(step: BuildStep): number` — use a match expression to return the estimated duration in seconds: Lint=5, Compile=30, Test=60, Package=15, Deploy=45
- `summarizeResult(step: BuildStep, code: ExitCode): string` — use a match expression on `code` to return: for `Success` return `"<step> passed"`, for `Warning` return `"<step> passed with warnings"`, for `Failure` return `"<step> FAILED"`. You'll need to convert the step enum value to its underlying string to interpolate it into the message.
- `shouldAbort(step: BuildStep, code: ExitCode): boolean` — use a match expression. Abort (return `true`) if the exit code is `Failure` and the step is `Compile` or `Deploy`. All other combinations should return `false`.
