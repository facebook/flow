// @flow

// Feature: multi params
function $pred$multi_param(w,x,y,z) {
  return typeof z === "string";
}

function foo(x: string | Array<string>): string {
  if ($pred$multi_param("1", "2", x, "3")) {
    return x;
  } else {
    return x.join();
  }
}
