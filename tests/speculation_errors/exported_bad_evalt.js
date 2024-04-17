declare export class C<T> {
  get(): T | null;
  get(v: T): T;
}
type Obj = {foo: string};
export type Obj2 = {...Obj}

export type Bad = C<Obj2[string]>;
