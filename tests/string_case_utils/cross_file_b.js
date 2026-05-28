import type {X, Y} from './cross_file_a';

'FOO' as X; // OK
'foo' as X; // ERROR
'BAR' as X; // ERROR

'foo' as Y; // OK
'FOO' as Y; // ERROR
