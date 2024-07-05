import type {V} from './a';

declare const ivs: $ReadOnlyArray<V>;
ivs.find(_ => true) as ?V;
