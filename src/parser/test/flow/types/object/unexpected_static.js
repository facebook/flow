// error: object types don't have static fields
type A = { static x: X }

// error: object types don't have static fields (including indexers)
type B = { static [K]: V }

// error: object types don't have static fields (including spread)
type C = { static ...X }

// ok: prop named static
type D = { static: X }

// error: static prop named static
type E = { static static: X }

// ok: parsed as a method named static
type F = { static(): R }

// ok: parsed as a poly method named static
type G = { static<X>(): R }

// error: static method named static
type H = { static static(): R }

// error: static poly method named static
type I = { static static<X>(): R }
