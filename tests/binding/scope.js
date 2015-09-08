function block_scope() {
  let a: number = 0;
  var b: number = 0;
  {
    let a = ""; // ok: local to block
    var b = ""; // error: string ~> number
  }
}

function switch_scope(x: string) {
  let a: number = 0;
  var b: number = 0;
  switch (x) {
    case "foo":
      let a = ""; // ok: local to switch
      var b = ""; // error: string ~> number
      break;
    case "bar":
      let a = ""; // TODO error: a already bound in switch
      break;
  }
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
  for (var a = "" /* error: string ~> number */;;) {}
}

function for_in_scope_let(o: Object) {
  let a: number = 0;
  for (let a /* ok: local to init */ in o) {}
}

function for_in_scope_var(o: Object) {
  var a: number = 0;
  for (var a /* error: string ~> number */ in o) {}
}

function for_of_scope_let(xs: string[]) {
  let a: number = 0;
  for (let a /* ok: local to init */ of xs) {}
}

function for_of_scope_var(xs: string[]) {
  var a: number = 0;
  for (var a /* error: string ~> number */ of xs) {}
}
