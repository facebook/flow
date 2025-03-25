const obj1 = {x: 1};
obj1.x as 1; // okay in partial mode

const x = 1;
const obj2 = {x};
obj2.x as 1; // error in partial mode
