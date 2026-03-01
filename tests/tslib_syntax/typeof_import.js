// `typeof import` should resolve module types
type A = typeof import("./exports").default;
