function switch_scope(x: mixed) {
  let a = "";
  let b: number | string = "";
  switch (x) {
    case "foo":
      let a;
      a = 0; // doesn't add lower bound to outer a
      b = 0;
  }
  a as string; // OK
  b as string; // error: number ~> string
}

function try_scope_finally() {
  let a;
  let b;
  try {
    a = "";
    b = "";
  } finally {
    let a;
    a = 0; // doesn't add lower bound to outer a
    b = 0;
  }
  a as string; // ok
  b as string; // error: number ~> string
}

function for_scope() {
  let a = "";
  let b: string | number = "";
  for (let a;;) {
    a = 0; // doesn't add lower bound to outer a
    b = 0;
  }
  a as string;
  b as string; // error: number ~> string
}

function for_in_scope(o: Object) {
  let a = 0;
  let b: number | string = 0;
  for (let a in o) {
    a = ""; // doesn't add lower bound to outer a
    b = "";
  }
  a as number;
  b as number; // error: string ~> number
}

function for_of_scope(xs: number[]) {
  let a = "";
  let b: number | string = "";
  for (let a of xs) {
    a = 0; // doesn't add lower bound to outer a
    b = 0;
  }
  a as string;
  b as string; // error: number ~> string
}
