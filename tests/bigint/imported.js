import {x, z, a, c} from './exported';
import y from './exported';

x as bigint; // ok
x as empty; // error

y as bigint; // ok
y as empty; // error

z as bigint; // ok
z as empty; // error

a as bigint; // ok
a as empty; // error

c as bigint; // ok
c as empty; // error
