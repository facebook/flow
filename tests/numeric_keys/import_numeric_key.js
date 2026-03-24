import obj from './export_numeric_key';

obj[1] as boolean; // OK
obj['1'] as boolean; // OK
obj[2] as string; // OK
obj['2'] as string; // OK
obj[1] as empty; // ERROR
obj[2] as empty; // ERROR
