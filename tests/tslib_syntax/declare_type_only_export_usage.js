import type {X, I} from './declare_type_only_export';

'oops' as X; // ERROR - string is not number
({}) as I; // ERROR - missing baz
