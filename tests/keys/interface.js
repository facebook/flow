interface WithDict {
  ['x' | 'y']: number;
}

interface WithProps {
  a: number;
  method(): void;
}

interface WithBoth {
  ['x' | 'y']: number;
  a: number;
  method(): void; // non-own and ignored
}

declare var xy: 'x' | 'y';

(xy: keyof WithDict); // OK
('xxx': keyof WithDict); // ERROR

('a': keyof WithProps); // OK
('xxx': keyof WithProps); // ERROR

('a': keyof WithBoth); // OK
(xy: keyof WithBoth); // OK
('xxx': keyof WithBoth); // ERROR
