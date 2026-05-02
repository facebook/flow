type Arr = Array<number>;
type Arr_Elem = Arr[number];

42 as Arr_Elem; // OK: `Arr_Elem` is `number`
'hello world' as Arr_Elem;

function foo(a: Arr): Arr[number] {
  if (false) return a[0];
  else return 0;
}

type Obj = { [key: string]: number };
type Obj_Elem = Obj[string];

42 as Obj_Elem; // OK: `Obj_Elem` is `number`
'hello world' as Obj_Elem;

function bar(o: Obj): Obj[string] {
  if (false) return o['buz'];
  else return 0;
}

type Z = ?{c: number, ...};
1 as Z['c']; // Error - access 'c' on `void`/`null`
1 as NonNullable<Z>['c']; // OK
