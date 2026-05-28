type PathImpl<T, K extends keyof T> =
  K extends string
  ? T[K] extends Record<string, any>
    ? K | `${K}.${PathImpl<T[K], keyof T[K]>}`
    : K
  : never;

type Path<T> = PathImpl<T, keyof T> | keyof T;

type PathValue<T, P extends Path<T>> =
  P extends `${infer K}.${infer Rest}`
  ? K extends keyof T
    ? Rest extends Path<T[K]>
      ? PathValue<T[K], Rest>
      : never
    : never
  : P extends keyof T
    ? T[P]
    : never;

declare function get<T, P extends Path<T>>(obj: T, path: P): PathValue<T, P>;

type Obj = {
  firstName: string;
  lastName: string;
  age: number;
  address: {
    city: string;
    state: {
      name: string;
      code: number;
    }
  }
};

declare const object: Obj;

get(object, "firstName") as string; // OK
get(object, "address") as {city: string, state: {name: string, code: number}}; // OK
get(object, "address.city") as string; // OK
get(object, "address.state.name") as string; // OK
get(object, "address.state.code") as number; // OK

get(object, "invalid"); // ERROR
get(object, "address.invalid"); // ERROR
