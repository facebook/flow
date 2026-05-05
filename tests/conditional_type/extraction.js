function extract_arr_element() {
  type Extract<T> = T extends Array<infer X> ? X : false;
  0 as Extract<Array<number>>; // ok
  0 as Extract<Array<string>>; // error: number ~> string
  true as Extract<3>; // error: true ~> false
}

function extract_arr_element_with_bound() {
  type Extract<T> = T extends Array<infer X extends number> ? X : boolean;
  0 as Extract<Array<number>>; // ok
  '0' as Extract<Array<number>>; // error: string ~> number
  0 as Extract<Array<string>>; // error: number ~> boolean
}

function extract_return_type() {
  3 as ReturnType<() => number>; // ok
  3 as ReturnType<() => string>; // error: number ~> string
  3 as ReturnType<(number) => number>; // ok
  3 as ReturnType<(number) => string>; // error: number ~> string
  3 as ReturnType<(number, string, ...any) => number>; // ok
  3 as ReturnType<(number, string, ...any) => string>; // error: number ~> string
}

function extract_parameters_type() {
  3 as Parameters<() => number>[0]; // error: invalid-tuple-index
  '' as Parameters<(string, number) => string>[0]; // ok
  3 as Parameters<(string, number) => string>[0]; // error: number ~> string
  '' as Parameters<(string, number) => string>[1]; // error: string ~> number
  3 as Parameters<(string, number) => string>[1]; // ok
}

function extract_this_parameter_type() {
  3 as ThisParameterType<(this: number, string) => void>; // ok
  '' as ThisParameterType<(this: number, string) => void>; // error: string ~> number

  // This param is inferred as any,
  // which comes from unsoundness of this param in function type annotation.
  3 as unknown as ThisParameterType<(string, number) => string>; // ok

  '3' as OmitThisParameter<ThisParameterType<(this: number, string) => void>>; // ok: fn type with this type has an implicit any this type
}

function recursive_awaited_type() {
  // A simplied version of TS's Awaited type by focusing on nonimal Promise type only,
  // but we still exercise the recursive conditional type behavior.
  type Awaited<T> = T extends Promise<infer F> ? Awaited<F> : T;

  declare var awaited_null: Awaited<null>;
  declare var awaited_n1: Awaited<number>;
  declare var awaited_n2: Awaited<Promise<number>>;
  declare var awaited_n3: Awaited<Promise<Promise<Promise<Promise<number>>>>>;
  awaited_null as empty; // error: null ~> empty
  awaited_n1 as empty; // error: number ~> empty
  awaited_n2 as empty; // error: number ~> empty
  awaited_n3 as empty; // error: number ~> empty
}

function excluded_first_rest_params() {
  type RestParams<T: (...args: ReadonlyArray<empty>) => unknown> =
  T extends (fst: any, ...args: infer Args) => any ? Args : null;
  declare function foobar(a: number, b: boolean, c: string, d: number): void;
  const [b1, s1, n1]: RestParams<typeof foobar> = [true, 'hello', 123]; // ok
  const [b2, s2, n2]: RestParams<typeof foobar> = [123, true, 'hello']; // error
}
