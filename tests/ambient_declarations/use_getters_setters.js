import {Person} from './getters_setters.js.flow';
import {AccessorNS} from './getters_setters_namespace';

// Test Person getters/setters
const p = new Person();

// Reading getters - should work
const name: string = p.name;
const age: number = p.age;

// Writing setter - should work
p.name = "Alice";

// Type errors
const badName: number = p.name; // ERROR: string ~> number
const badAge: string = p.age; // ERROR: number ~> string
p.name = 123; // ERROR: number ~> string

// Test static getters/setters
const count: number = Person.count;
Person.count = 10;
const badCount: string = Person.count; // ERROR: number ~> string
Person.count = "not a number"; // ERROR: string ~> number

// Test namespace getters/setters
const item = new AccessorNS.Item();

// Reading getters - should work
const id: number = item.id;
const label: string = item.label;

// Writing setter - should work
item.id = 42;

// Type errors
const badId: string = item.id; // ERROR: number ~> string
const badLabel: number = item.label; // ERROR: string ~> number
item.id = "not a number"; // ERROR: string ~> number

// Test static namespace getters/setters
const defaultLabel: string = AccessorNS.Item.defaultLabel;
const badDefaultLabel: number = AccessorNS.Item.defaultLabel; // ERROR: string ~> number
