function f1(x: any): x is number { return true; }
function f2(x: any): x is number => number { return true; }
function f3(x: any): x is (number) => number { return true; }
function f4(x: any): x is (y: number) => number { return true; }
function f5(x: any): x is number => x is number { return true; }
