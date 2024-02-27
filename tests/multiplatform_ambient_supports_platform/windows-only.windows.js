import { foo } from "mobile-and-win";

foo as number; // ok: resolves to mobile-and-win.windows.js
foo as string; // error: doesn't resolve to resolves to mobile-and-win.js.flow
