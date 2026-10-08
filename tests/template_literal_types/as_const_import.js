import {exported_suffix, exported_folded, exported_const_suffix, exported_const_folded} from './as_const';

exported_suffix as `${string}dp`; // OK
exported_suffix as string; // OK
exported_suffix as `${string}px`; // ERROR

exported_folded as "hello world"; // OK
exported_folded as string; // OK
exported_folded as "hello mars"; // ERROR

exported_const_suffix as `${string}dp`; // OK
exported_const_suffix as string; // OK
exported_const_suffix as `${string}px`; // ERROR

exported_const_folded as "hello world"; // OK
exported_const_folded as string; // OK
exported_const_folded as "hello mars"; // ERROR
