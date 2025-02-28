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
