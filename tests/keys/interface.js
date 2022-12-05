// @flow

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

(xy: $Keys<WithDict>); // OK
('xxx': $Keys<WithDict>); // ERROR

('a': $Keys<WithProps>); // OK
('xxx': $Keys<WithProps>); // ERROR

('a': $Keys<WithBoth>); // OK
(xy: $Keys<WithBoth>); // OK
('xxx': $Keys<WithBoth>); // ERROR
