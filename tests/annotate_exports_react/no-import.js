// @flow

import { foo } from "./lib";
import { bar } from "./fake-react";

export function my_foo() {
  return foo();
}
export function my_bar() {
  return bar();
}
