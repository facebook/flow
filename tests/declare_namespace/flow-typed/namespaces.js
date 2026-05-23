declare namespace ns_v {
  declare const a: string;
}
declare namespace ns_v {
  declare const b: string;
}
declare namespace ns_v {
  type a = string; // error: already bound
  type b = string; // error: already bound
}
declare namespace ns_t {
  type T1 = string;
}
declare namespace ns_t {
  type T2 = string;
}
declare namespace ns_v_and_then_t {
  declare const a: string;
}
declare namespace ns_t_and_then_v {
  type T1 = string;
}
declare namespace ns_v_and_then_t {
  type T1 = string;
}
declare namespace ns_t_and_then_v {
  declare const a: string;
}

declare namespace ns_decl_merge_interface {
  interface Box { a: string }
}
declare namespace ns_decl_merge_interface {
  interface Box { b: number }
}

declare namespace ns_decl_merge_class {
  declare class Box { a: string }
}
declare namespace ns_decl_merge_class {
  interface Box { b: number }
}

declare namespace ns_decl_merge_class_reverse {
  interface Box { a: string }
}
declare namespace ns_decl_merge_class_reverse {
  declare class Box { b: number }
}

declare namespace ns_decl_merge_overload {
  declare function f(x: string): string;
}
declare namespace ns_decl_merge_overload {
  declare function f(x: number): number;
}

declare namespace ns_decl_merge_nested {
  declare namespace Inner {
    type A = string;
  }
}
declare namespace ns_decl_merge_nested {
  declare namespace Inner {
    type B = number;
  }
}

declare namespace ns_decl_merge_type_then_value_namespace {
  declare namespace Inner {
    type A = string;
  }
}
declare namespace ns_decl_merge_type_then_value_namespace {
  declare namespace Inner {
    declare const x: number;
  }
}

declare namespace ns_decl_merge_fn_namespace {
  declare function f(): void;
}
declare namespace ns_decl_merge_fn_namespace {
  declare namespace f {
    type T = string;
  }
}

declare namespace ns_decl_merge_class_namespace {
  declare class C {}
}
declare namespace ns_decl_merge_class_namespace {
  declare namespace C {
    type T = string;
  }
}

declare const non_ns_value: string;
type non_ns_type = string;
// The following namespaces won't have any effect
declare namespace non_ns_value {
  declare const b: string;
}
declare namespace non_ns_value {
  type T1 = string;
}
declare namespace non_ns_type {
  declare const b: string;
}
declare namespace non_ns_type {
  type T1 = string;
}
