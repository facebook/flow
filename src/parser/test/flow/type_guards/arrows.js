const f1 = (x: any): x is true => true;
const f2 = (x: any): x is (x: true) => true => true;
const f3 = (x: any): x is (x: true) => x is true => true;
const f4 = (x: any): x is (x: true) => x is (x: true) => x is true => true;
