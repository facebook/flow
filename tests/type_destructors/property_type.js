type AnyNumber = any[number];

type Obj = { x: string };
type Obj_Prop_x = Obj['x'];

(42: Obj_Prop_x);

function foo(o: Obj): Obj['x'] {
  if (false) return o.x;
  else return 0;
}
