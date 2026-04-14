import {MyType} from './exporter';
import {MyInterface} from './exporter';
import {MyNamespace} from './exporter';
declare const x: MyType;
x as number; // OK
x as string; // ERROR: number ~> string (proves the type was resolved)
declare const y: MyInterface;
y.x as string; // OK
y.x as number; // ERROR: string ~> number (proves the interface resolved)
const z = MyType; // ERROR: type used as value
const w = MyInterface; // ERROR: type used as value
const ns = MyNamespace; // ERROR: type used as value
