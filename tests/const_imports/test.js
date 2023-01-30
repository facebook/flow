import {named} from "./dep.js"; named = 43; // error
import def from "./dep.js"; def = "nope"; // error
import * as ns from "./dep.js"; ns = {}; // error
