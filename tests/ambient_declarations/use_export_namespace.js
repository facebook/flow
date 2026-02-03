import {ExportedNS} from './export_namespace.js.flow';

// Test value members of exported namespace
ExportedNS.x as empty; // error: number ~> empty
ExportedNS.foo(); // ok

// Test type members of exported namespace
1 as ExportedNS.T; // error: number ~> string
'' as ExportedNS.T; // ok
