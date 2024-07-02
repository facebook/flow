import {data} from './prefix';

// Type sig from importing
data as StringPrefix<'data'>; // OK
data as string; // OK
data as 'data-'; // ERROR
