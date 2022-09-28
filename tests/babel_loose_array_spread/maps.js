const map1 = new Map<string, string>();
const map2 = new Map<string, string>();
new Map<mixed, mixed>([
  ...map1, // Error
  ...map2 // Error
]);
new Map([...Array.from(map1), ...Array.from(map2)]); // No error
f(
  ...map1, // Error
  ...map2 // Error
);
function f(...args: Array<mixed>) {}
