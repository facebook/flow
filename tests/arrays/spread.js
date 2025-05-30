declare function foo<TArguments: $ReadOnlyArray<{foo: 'a'}>, TReturn>(
    implementation?: (...args: TArguments) => TReturn,
  ): (...args: TArguments) => TReturn

type Config<Return> = {|
  +control:  () => {foo: 'a'},
  +areEqual: (control: Return, test: Return) => boolean,
|};

declare function bar(config: Config<{foo: 'a'}>) : void

bar({
  control: () => ({foo: 'a'}),
  areEqual : foo((control, test) => control.foo === test.foo),
});

let x = [3, 4] as const;
let y = [3, "a"] as const;
let z = ["a"] as const;

([...x][0] : 3);
([...x][1] : 4);
([...x, ...z][0] : 3);
([...x, ...z][1] : 4);
([...x, ...z][2] : "a");
([...x, ...y][0] : 3);
([...x, ...y][1] : 4);
([...x, ...y][2] : 3);
([...x, ...y][3] : "a");
([...x, ...y, ...z][0] : 3);
([...x, ...y, ...z][1] : 4);
([...x, ...y, ...z][2] : 3);
([...x, ...y, ...z][3] : "a");
([...x, ...y, ...z][4] : "a");

([...z, ...z][0] : "a");
([...z, ...z][1] : "a");

([...x, ...[3, 4]][0] : 3);
([...x, ...[3, 4]][1] : 4);

([...x, ...x][0] : 3); // error
([...x, ...x][1] : 4); // error

([...x, ...x, ...y][0] : 3); // error
([...x, ...x, ...y][1] : 4); // error
([...x, ...x, 1][0] : 3); // error

let three = 3 as const;
let a = [three, three];
let b = [three, 4] as const;
([...a, ...b][0] : 3);
([...a, ...b][1] : 3);
([...a, ...b][2] : 3);
([...a, ...b][3] : 4);

function foo(a: [1,2,3], b: [4,5,6]): [1,2,3,4,5,6] {
  return [...a, ...b];
}

function bar(a: [1,2,3], b: [4,5,6]): [1,2,3,1,2,3] {
  return [...a, ...a];
}

let emp = [];
emp.push(42);
([...emp]: Array<string>); // error
