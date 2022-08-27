function block_scope() {
  let a: number = 0;
  var b: number = 0;
  {
    let a = ""; // ok: local to block
    var b = ""; // error: banned redeclaration
  }
}

function switch_scope(x: string) {
  let a: number = 0;
  var b: number = 0;
  switch (x) {
    case "foo":
      let a = ""; // ok: local to switch
      var b = ""; // error: banned redeclaration
      break;
    case "bar":
      let a = ""; // error: a already bound in switch
      break;
  }
}

// a switch is a single lexical scope, so lets and non-disjoint
// cases can mix in odd ways. our support for edge cases is not
// yet perfect.
function switch_scope2(x: number) {
  switch (x) {
    case 0:
     a = "";     // error: assign before declaration
     break;
    case 1:
     var b = a;  // error: use before declaration
     break;
    case 2:
      let a = "";
      break;
    case 3:
      a = "";     // error: skipped initializer
      break;
    case 4:
      var c:string = a;  // error: skipped initializer
      break;
  }
  a = ""; // error: a no longer in scope
}

function try_scope() {
  let a: number = 0;
  try {
    let a = ""; // ok
  } catch (e) {
    let a = ""; // ok
  } finally {
    let a = ""; // ok
  }
}

function for_scope_let() {
  let a: number = 0;
  for (let a = "" /* ok: local to init */;;) {}
}

function for_scope_var() {
  var a: number = 0;
  for (var a = "" /* error: banned redeclaration */ ;;) {}
}

function for_in_scope_let(o: Object) {
  let a: number = 0;
  for (let a /* ok: local to init */ in o) {}
}

function for_in_scope_var(o: Object) {
  var a: number = 0;
  for (var a /* error: banned redeclaration */ in o) {}
}

function for_of_scope_let(xs: string[]) {
  let a: number = 0;
  for (let a /* ok: local to init */ of xs) {}
}

function for_of_scope_var(xs: string[]) {
  var a: number = 0;
  for (var a /* error: banned redeclaration */ of xs) {}
}

function default_param_1() {
  // function binding in scope in default expr
  function f(
    x: () => string = f // error: number ~> string
  ): number {
    return 0;
  }
}

function default_param_2() {
  // fn body bindings not visible from param scope
  let a = "";
  function f0(x: () => string = () => a): number {
    let a = 0;
    return x(); // error: string ~> number
  }
  function f1(x: number = b /* error: cannot resolve b */): number {
    let b = 0;
    return x;
  }
}

/**
 * TODO:
 * The old env has the implementation-defined behavior that considers function parameter annotations
 * to be in the outer scope of function parameters and bodies.
 * This is confusing, and we should eventually fix it when the new-env rolls out.
 */
function function_type_annotation_scope() {
  const x: string = "";
  const y: number = 0;
  const z: boolean = true;
  // Function parameter annotations are in the parent scope of function parameters.
  function f(x: number, y: boolean, z: string, a: typeof x, b: typeof y, c: typeof z) {
    (a: string); // OK
    (b: number); // OK
    (c: boolean); // OK
  }

  function invalid_self_reference(
    selfRef: typeof selfRef // Error: cannot resolve selfRef
  ) {}

  function invalid_forward_reference_typeof(
    x: typeof forwardRef, // Error: cannot resolve forwardRef
    forwardRef: number,
  ) {}

  function annotations_in_default(
    x: number,
    y: typeof x,
    f: (number) => void = (z: typeof x) => { (z: number) }
  ) {
    (y: string);
  }
}
