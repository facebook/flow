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

// ok: method named static
type F = { static(): R }

// ok: generic method named static
type G = { static<X>(): R }
