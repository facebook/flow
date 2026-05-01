// In a .js file, these are inert comments — neither suppresses, neither warns.

// @ts-expect-error
3 as string; // ERROR: not suppressed (file is .js)

// @ts-ignore
3 as string; // ERROR: not suppressed (file is .js)
