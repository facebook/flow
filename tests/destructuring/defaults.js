/* @flow */

function obj_prop_fun({p:{q=0}={q:true}}: {p?:{q?:number|string}} = {p:{q:""}}) {
  // errors:
  // * number | string ~> void, from parameter annotation
  // * boolean ~> void, from default on _.p
  (q:void);
}
obj_prop_fun(); // ok
obj_prop_fun({}); // ok
obj_prop_fun({p:{}}); // ok

function obj_prop_var(o: {p?: {q?:number|string}} = {p:{q:""}}) {
  var {p:{q=0}={q:true}} = o;
  // errors:
  // * number | string ~> void, from parameter annotation
  // * boolean ~> void, from default on o.p
  (q:void);
}
obj_prop_var(); // ok
obj_prop_var({}); // ok
obj_prop_var({p:{}}); // ok

function obj_rest({p:{q,...o}={q:0,r:0}}: {p?: {r?: string}} = {p:{q:0,r:""}}) {
  // errors:
  // * number  ~> void, from default on _.p
  // * string  ~> void, from parameter annotation
  (o.r:void);
}
obj_rest(); // ok
obj_rest({}); // ok
obj_rest({p:{}}); // ok

function obj_prop_annot({
  p = true // error: boolean ~> string
}: {
  p: string
} = {
  p: 0 // error: number ~> string
}) {
  (p:void); // error: string ~> void
}

var {
  p = true // error: boolean ~> string
}: {
  p: string
} = {
  p: 0 // error: number ~> string
};
(p:void); // error: string ~> void

function obj_prop_err({x:{y}}:null=null) {} // error: property `x` cannot be accessed on null
function obj_rest_err({...o}:number=0) {} // error: expected object instead of number
function arr_elem_err([x]:null=null) {} // error: element 0 cannot be accessed on null
function arr_rest_err([...a]:null=null) {} // error: expected array instead of null

function gen<T>(x:T,{p=x}:{p:T}):T {
  return p;
}

// Default values in destructuring unwrap optional types
obj_prop_fun(({} : {p?:{q?:number|string}})); // ok
obj_prop_var(({} : {p?:{q?:number|string}})); // ok

// union-like upper bounds preserved through destructuring
function obj_prop_opt({p}:{p?:string}={p:0}) {}
function obj_prop_maybe({p}:{p:?string}={p:0}) {}
function obj_prop_union({p}:{p:number|string}={p:true}) {}

// union-of-objects upper bounds preserved through destructuring
function obj_prop_union2({p}:{p:number}|{p:string}={p:true}) {}

function default_expr_scope({a, b = a}: {a: string, b?: string}) {}

function Component({
  name = 123456, // Not string type, error!
}: {| name?: string |}) {
  (name: string);
}
