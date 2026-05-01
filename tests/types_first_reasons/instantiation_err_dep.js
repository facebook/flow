import type {U} from './instantiation_err';
0 as U; // forces the lazy TypeAppT, which introduces an error
