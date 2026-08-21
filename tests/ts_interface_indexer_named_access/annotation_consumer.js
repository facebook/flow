import {tsRead, flowRead} from './annotation_position';
import type {TSAlias} from './annotation_position';

tsRead as number;
tsRead as string; // ERROR: the indexer value is a number
flowRead as number;

declare const alias: TSAlias;
alias as string; // ERROR: the indexer value is a number

// Resolved in `ts_annotation_position.ts`, where the lookup is TypeScript.
import {flowReadInTs} from './ts_annotation_position';
import type {FlowAliasInTs} from './ts_annotation_position';

flowReadInTs as number;
flowReadInTs as string; // ERROR: the indexer value is a number

declare const aliasInTs: FlowAliasInTs;
aliasInTs as string; // ERROR: the indexer value is a number
