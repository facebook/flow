// A union of objects keyed by distinct `unique symbol`s must keep both members
// rather than collapsing them by display name. Named symbols render as `[a]` /
// `[b]`; unnamed ones both render as `[symbol]` but remain distinct members.
import {Keys} from './syms';

declare const named: {[Keys.a]: number} | {[Keys.b]: number};
named;
//^

declare const s1: unique symbol;
declare const s2: unique symbol;
declare const unnamed: {[s1]: number} | {[s2]: number};
unnamed;
//^
