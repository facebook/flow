/**
 * @format
 * @flow
 */

declare var any: any;

any as (x: {p: number}) => void as (x: {p: string}) => void;

type X<-T> = mixed;
any as X<{p: number}> as X<{p: string}>;
