function f(a: string): void;
function f(a: number): void;
function f(a: string | number): void {}

export function g<T>(a: T): T;
export function g(a: string): string;
export function g(a: string): string { return a; }

export default function h(a: string): string;
export default function h(a: string): string { return a; }

function destructured({a}: {a: string}): void;
function newlineBody(a: string): string
{ return a; }
function asi(a: string): void
function eof(a: number): number
