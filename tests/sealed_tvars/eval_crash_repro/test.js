import type {V} from './a';

declare const ivs: ReadonlyArray<V>;
ivs.find(_ => true) as ?V;
