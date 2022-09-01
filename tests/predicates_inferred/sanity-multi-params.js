// @flow

// Feature: multi params
function multi_param(w: mixed,x: mixed,y: mixed,z: mixed): %checks {
  return typeof z === "string";
}

function foo(x: string | Array<string>): string {
  if (multi_param("1", "2", x, "3")) {
    return x;
  } else {
    return x.join();
  }
}
