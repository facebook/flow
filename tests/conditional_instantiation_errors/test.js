declare const valid: Flatten<[[number]], 1>;
valid as number;
valid as string; // ERROR

declare const stopped: Flatten<[[number]], -1>;
stopped as [[number]];

declare const broad: Flatten<[[[number]]], number>; // ERROR
broad as number | [number] | [[number]];

declare const trueBranch: TrueBranch<2>; // ERROR
trueBranch as number;

declare const falseBranch: FalseBranch<2>; // ERROR
falseBranch as number;
