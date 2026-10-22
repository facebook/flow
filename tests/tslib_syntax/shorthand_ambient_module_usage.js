import Explicit from 'shorthand-explicit';
import * as explicitNamespace from 'shorthand-explicit';
import { value as explicitValue } from 'shorthand-explicit'; // ERROR
import { value as implicitValue } from 'shorthand-implicit'; // ERROR
import { value as emptyBodyValue } from 'shorthand-empty-body'; // ERROR

Explicit as {};
Explicit as number; // ERROR
explicitNamespace.value; // ERROR
