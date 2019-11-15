const it: Iterable<number> = [7,8,9];
[...it]; // Error
f(...it); // Error
f.bind(null, ...it); // Error
if (Array.isArray(it)) {
  [...it]; // No error
  f(...it); // No error
  f.bind(null, ...it); // No error
}

function f(...args) {}
