// @ts-ignore suppresses the next-line error
3 as string;

// @ts-ignore — unused, must NOT produce EUnusedSuppression
3 as number;

// stack mixing @ts-ignore with @ts-expect-error: any @ts-ignore silences the stack
// @ts-ignore
// @ts-expect-error
3 as number;

// stack of @ts-ignore over $FlowExpectedError[code]: wildcard absorbs and silences
// @ts-ignore
// $FlowExpectedError[incompatible-cast]
3 as number;
