ns_v.a as empty; // error: string ~> empty
ns_v.b as empty; // error: string ~> empty
ns_t; // error: it's a type only namespace
1 as ns_t.T1; // error: number ~> string
ns_v_and_then_t as empty; // error: value namespace ~> empty
ns_t_and_then_v as empty; // error: value namespace ~> empty

non_ns_value.b as empty; // error: value namespace ~> empty
non_ns_type; // error: it's a type
type _t2 = non_ns_type; // ok
