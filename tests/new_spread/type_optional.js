declare class T {}
declare class U {}

declare const a: {...{ p :T, ... },...{ p :U, ... }, ...}; a as { p:U, ... };
declare const b: {...{ p?:T, ... },...{ p :U, ... }, ...}; b as { p:U, ... };
declare const c: {...{ p :T, ... },...{ p?:U, ... }, ...}; c as { p:T|U, ... };
declare const d: {...{ p?:T, ... },...{ p?:U, ... }, ...}; d as { p?:T|U, ... };

declare const e: {...{p :T},...{ p :U, ... }, ...}; e as { p :U, ... };
declare const f: {...{p?:T},...{ p :U, ... }, ...}; f as { p:U, ... };
declare const g: {...{p :T},...{ p?:U, ... }, ...}; g as { p :T|U, ... };
declare const h: {...{p?:T},...{ p?:U, ... }, ...}; h as { p?:T|U, ... };

declare const i: {...{ p :T, ... },...{p :U}, ...}; i as { p :  U, ... };
declare const j: {...{ p?:T, ... },...{p :U}, ...}; j as { p :  U, ... };
declare const k: {...{ p :T, ... },...{p?:U}, ...}; k as { p:T|U, ... };
declare const l: {...{ p?:T, ... },...{p?:U}, ...}; l as { p?:T|U, ... };

declare const m: {...{p :T},...{p :U}}; m as {p :  U};
declare const n: {...{p?:T},...{p :U}}; n as {p :  U};
declare const o: {...{p :T},...{p?:U}}; o as {p :T|U};
declare const p: {...{p?:T},...{p?:U}}; p as {p?:T|U};
