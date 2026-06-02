/**
 * @format
 * @flow
 */

declare const any: any;

any as (x: {p: number, ...}) => void as (x: {p: string, ...}) => void;

type X<in T> = unknown;
any as X<{p: number, ...}> as X<{p: string, ...}>;
