import { foo } from "CommonLib";

foo as string;
foo as empty; // error: string ~> empty

declare export const native: string;

import 'WebInCommon' // error: native code cannot import web code. This one tests the precedence rule in flowconfig
