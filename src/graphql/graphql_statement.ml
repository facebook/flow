type context = {
  mutable vars: Graphql_schema.Type.t SMap.t;
  infer_vars: bool;
}

let mk_context ?vars:(vars=SMap.empty) ?infer_vars:(infer_vars=false) () =
  { vars; infer_vars }
