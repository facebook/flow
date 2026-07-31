// Without `experimental.importable_global_libdefs`, resolution never reaches the
// libdef, so importing it reports the pre-existing cannot-resolve-module error.
import "./globals"; // import error
import {MyGlobal as Imported} from "./globals"; // import error

module.exports = {Imported};
