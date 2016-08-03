type Malformed = $PropertyType<any, number>;

type Obj = { x: string };
type Obj_Prop_x = $PropertyType<Obj, 'x'>;

(42: Obj_Prop_x);
