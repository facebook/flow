import { native } from "NativeOnly"; // error: common code cannot import native-only code
import { web } from "WebOnly"; // ok: common code can import web-only code, because web-only code is treated as implicit common interface. TODO: common interface validation is missing for now

web as empty; // error: string ~> empty
native as empty;
