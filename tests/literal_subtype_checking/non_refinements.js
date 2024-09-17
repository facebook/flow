type T = 'foo' | 'bar';

declare const x: ?T;

x === 'foox'; // ERROR

x === 'foox' || x === 'barx'; // ERROR

x === 'foox' || true;

true || x === 'foox'; // ERROR

x != null && (x === 'foox' || x === 'barx'); // ERROR
