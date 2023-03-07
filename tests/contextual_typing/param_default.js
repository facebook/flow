declare var context: (a?: number) => void;
context = (a) => {(a: number)}; // error: number | void ~> number
context = (a?) => {(a: number)}; // error: number | void ~> number
context = (a = 1) => {(a: number)}; // ok
context = (a? = 1) => {(a: number)}; // ok

// Safety is maintained by banning default to be nullable
context = (a = undefined) => {(a: number)}; // error: void ~> number

((a = null) => {}: (a: string | null) => void); // ok: we only filter out void, not null
