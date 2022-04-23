//@flow

import {x as y, type T} from "./export";
import X from "./export";
import * as Y from "./export";

var w: T = "a"; // err
(y: empty); // err
(X: empty); // err
(Y.x: empty); // err
