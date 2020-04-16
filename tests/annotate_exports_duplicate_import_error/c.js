// @flow

import { foo as foo1 } from "./a1";
import { foo as foo2 } from "./a2";

module.exports = () => [foo1(), foo2()]; // duplicate import in same type
