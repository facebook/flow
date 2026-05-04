import type {LogMap} from './utils';
import {f} from './logger';

declare const sourceData: Readonly<{post_id: string, source_id: string, ...}> & LogMap<string, string>;

// (A & B) <: B <: ?B should hold even when B is imported from another module.
f(sourceData); // OK
