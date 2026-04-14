import {MyType} from './exporter_js'; // ERROR: import-type-as-value
declare const x: MyType;
x as number; // OK (still resolves for downstream usage despite error)
