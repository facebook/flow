import type {Idx, Key} from './symbol_index_exported';

declare const gen: symbol;
declare const k: Key;

const out1: symbol = k; // OK: the key set crosses the boundary as `symbol`
const out2: string = k; // ERROR: it is `symbol`, not `string`

const in1: keyof Idx = gen; // OK: any symbol is a key of a symbol indexer
const in2: keyof Idx = 'x'; // ERROR: a string is not

declare const idx: Idx;
const ks: Array<empty> = Object.keys(idx); // OK: no runtime key
