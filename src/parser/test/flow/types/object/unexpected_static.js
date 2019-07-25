// error: object types don't have static fields
type A = { static x: X }

// error: object types don't have static fields (including indexers)
type B = { static [K]: V }

// error: object types don't have static fields (including spread)
type C = { static ...X }

// error: object types don't have static fields (including mapped types)
type D = { static [K in V]: string }

// ok: prop named static
type E = { static: X }

// error: static prop named static
type F = { static static: X }

// ok: parsed as a method named static
type G = { static(): R }

// ok: parsed as a poly method named static
type H = { static<X>(): R }

// error: static method named static
type I = { static static(): R }

// error: static poly method named static
type J = { static static<X>(): R }
