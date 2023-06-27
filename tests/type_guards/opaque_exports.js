export type A<X: string> = {
  data: X,
  tag: "a"
};
export type B = {
  tag: "b"
};
export type C = {
  tag: "c"
};

export type As = A<string>;
export opaque type OpaqueB = B;
export opaque type OpaqueC = C;
export opaque type OpaqueAs = As;
export opaque type OpaqueT = void | OpaqueB | OpaqueC | OpaqueAs;
export type OpaqueOrString = OpaqueT | string;
