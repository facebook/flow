import {exported_suffix, exported_folded} from './as_const';

exported_suffix as `${string}dp`; // OK
exported_suffix as string; // OK
exported_suffix as `${string}px`; // ERROR

exported_folded as "hello world"; // OK
exported_folded as string; // OK
exported_folded as "hello mars"; // ERROR
