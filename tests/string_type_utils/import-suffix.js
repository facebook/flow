import {percent} from './suffix';

// Type sig from importing
percent as StringSuffix<'%'>; // OK
percent as string; // OK
percent as empty; // ERROR
