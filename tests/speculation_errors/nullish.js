/**
 * @format
 * @flow
 */

42 as string | null | void; // Error: should only show string
42 as {} | null | void; // Error: should only show object
42 as [] | null | void; // Error: should only show array
({}) as string | null | void; // Error: should only show string
({}) as {p: empty} | null | void; // Error: should only show object
({}) as [] | null | void; // Error: should only show array
[] as string | null | void; // Error: should only show string
[] as {} | null | void; // Error: should only show object
[] as [empty] | null | void; // Error: should only show array
