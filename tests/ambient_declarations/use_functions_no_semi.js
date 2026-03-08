import {cacheLife, noSemi} from './functions_no_semi';

cacheLife('default'); // OK
cacheLife('seconds'); // OK
cacheLife('minutes'); // OK
cacheLife('invalid'); // ERROR

noSemi(1) as string; // OK
noSemi(1) as empty; // ERROR
