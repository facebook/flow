import type {T1, T2} from './export';

1 as T1;
3 as T1; // error 3 ~> 1 | 2
({f: 1} as T2);
({f: 3} as T2); // error {f: 3} ~> {f: 1} | {f: 2}
