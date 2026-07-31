// A global libdef is not a module, so importing it is an error even though the
// file resolves. Both the side-effect and named import forms report it.
import "./globals"; // import error
import {MyGlobal as Imported} from "./globals"; // import error

module.exports = {Imported};
