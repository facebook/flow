//@flow

var x = 42;
x as string; // num </: string

var y: number = 'a'; // error
y as string; // err

var [a, ...rest1] = [1, 2, 3];
a as string; // num </: string
rest1[0] as string; // num </: string

var {w, ...rest2} = {w: 42, p: 100};
w as string; // err
rest2 as {p: number};
rest2.p as string; //err

try {
} catch (e) {
  e as empty; // fine, e is any
  e = 10; // fine, e is any
}

for (var of_ of [1, 2, 3]) {
  of_ as string; //err
  of_ as number;
}

for (var in_ in {a: 42}) {
  in_ as number; //err
  in_ as string;
}
