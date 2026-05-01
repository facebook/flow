class A {
  aOne: boolean;

  aMethod(): void {
  }
}

class B extends A {
  bOne: string;
  bTwo: number;

  bMethod(): void {
  }
}

type T = keyof B;

'bOne' as T; // OK
'bTwo' as T; // OK

'missing' as T; // Error

'bMethod' as T; // Error: methods are on proto

'aOne' as T; // Error: non-own field
'aMethod' as T; // Error: non-own field
