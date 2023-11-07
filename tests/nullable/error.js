/**
 * @format
 * @flow
 */

declare var any: any;

any as ?number as number;
any as ?number as null;
any as ?number as void;

any as number as ?number;
any as null as ?number;
any as void as ?number;

any as ?number as number | null;
any as ?number as number | void;

any as number | null as ?number;
any as number | void as ?number;
