ns_v.a as empty; // error: string ~> empty
ns_v.b as empty; // error: string ~> empty
ns_t; // error: it's a type only namespace
1 as ns_t.T1; // error: number ~> string
ns_v_and_then_t as empty; // error: value namespace ~> empty
ns_t_and_then_v as empty; // error: value namespace ~> empty

({a: "", b: 1}) as ns_decl_merge_interface.Box; // ok
({a: "", b: ""}) as ns_decl_merge_interface.Box; // error: string ~> number

(new ns_decl_merge_class.Box()).b as number; // ok
(new ns_decl_merge_class.Box()).b as string; // error: number ~> string
(new ns_decl_merge_class_reverse.Box()).a as string; // ok
(new ns_decl_merge_class_reverse.Box()).a as number; // error: string ~> number

ns_decl_merge_overload.f("") as string; // ok
ns_decl_merge_overload.f(1) as number; // ok
ns_decl_merge_overload.f(1) as string; // error: number ~> string

"" as ns_decl_merge_nested.Inner.A; // ok
1 as ns_decl_merge_nested.Inner.B; // ok
1 as ns_decl_merge_nested.Inner.A; // error: number ~> string

"" as ns_decl_merge_type_then_value_namespace.Inner.A; // ok
ns_decl_merge_type_then_value_namespace.Inner.x as number; // ok
ns_decl_merge_type_then_value_namespace.Inner.x as string; // error: number ~> string

"" as ns_decl_merge_fn_namespace.f.T; // ok
1 as ns_decl_merge_fn_namespace.f.T; // error: number ~> string
"" as ns_decl_merge_class_namespace.C.T; // ok
1 as ns_decl_merge_class_namespace.C.T; // error: number ~> string

non_ns_value.b as empty; // error: value namespace ~> empty
non_ns_type; // error: it's a type
type _t2 = non_ns_type; // ok
