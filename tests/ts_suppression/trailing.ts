// @ts-expect-error this trailing message is fine
3 as string;

// @ts-ignore another trailing message
3 as string;

// @ts-expect-error: colon-prefixed description is also fine
3 as string;

// @ts-ignore: colon-prefixed description is also fine
3 as string;
