// @ts-expect-error suppresses the next-line error
3 as string;

// @ts-expect-error
true as string;

// unused on purpose, must produce EUnusedSuppression
// @ts-expect-error ERROR: unused
3 as number;

// stack of two @ts-expect-error, both unused — must produce TWO unused warnings
// @ts-expect-error ERROR: unused
// @ts-expect-error ERROR: unused
3 as number;

// stack of two @ts-expect-error suppressing one error — must produce ZERO unused warnings
// @ts-expect-error
// @ts-expect-error
3 as string;

// interleaved with $FlowExpectedError
// @ts-expect-error
// $FlowExpectedError[incompatible-cast]
3 as string;
