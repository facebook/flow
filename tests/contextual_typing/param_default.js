declare var context: (a?: number) => void;
context = (a) => {a as number}; // error: number | void ~> number
context = (a?) => {a as number}; // error: number | void ~> number
context = (a = 1) => {a as number}; // ok
context = (a? = 1) => {a as number}; // ok

// Safety is maintained by banning default to be nullable
context = (a = undefined) => {a as number}; // error: void ~> number

((a = null) => {}) as (a: string | null) => void; // ok: we only filter out void, not null
