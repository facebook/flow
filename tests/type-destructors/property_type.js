// @flow

type Obj = { x: string };
type Obj_Prop_x = $PropertyType<Obj, 'x'>;

(42: Obj_Prop_x);

function foo(o: Obj): $PropertyType<Obj, 'x'> {
  if (false) return o.x;
  else return 0;
}

declare function get<O, K>(O, K): $PropertyType<O, K>;

(get({a: 'A'}, 'a'): 'B');
