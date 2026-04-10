import {foo} from './conformance';

declare const f: typeof foo;
f as string; // OK
f as empty; // ERROR
