// These are NOT directives — the prefix isn't followed by whitespace or EOS.
// They must NOT suppress and must NOT produce EUnusedSuppression.

// @ts-ignoree
3 as string; // ERROR: not suppressed

// @ts-expect-error-foo
3 as string; // ERROR: not suppressed

// @ts-ignorebar
3 as string; // ERROR: not suppressed
