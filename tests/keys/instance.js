// @flow

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

type T = $Keys<B>;

('bOne': T); // OK
('bTwo': T); // OK

('missing': T); // Error

('bMethod': T); // Error: methods are on proto

('aOne': T); // Error: non-own field
('aMethod': T); // Error: non-own field
