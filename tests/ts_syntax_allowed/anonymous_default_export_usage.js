import f from "./anonymous_default_export";

f("hello") as string; // OK
f(42) as number; // OK

f("hello") as number; // ERROR - string is not number
f(42) as string; // ERROR - number is not string
