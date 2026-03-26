import {MyClass} from './class_index_signature';

const obj: MyClass = new MyClass();
obj["foo"] as number; // OK: instance indexer returns number
obj["foo"] as string; // ERROR: number is not string
