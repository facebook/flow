import {l} from './let';
import {c} from './const';

l as "let"; // OK
l as empty; // ERROR

c as "const"; // OK
c as empty; // ERROR
