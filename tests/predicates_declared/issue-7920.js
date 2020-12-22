// @flow

import { _ } from "./issue-7920-lib";

const test: ?string = "foo";

if (!_.isNil(test)) {
  (test: string); // ok
  (null: typeof test); // error
}
