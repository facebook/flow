// Test using implicit namespace
import { ImplicitNS } from './implicit_namespace.js.flow';

// Access namespace members
const val: number = ImplicitNS.x;
ImplicitNS.foo();

// Type errors
const badVal: string = ImplicitNS.x; // ERROR: number not assignable to string
