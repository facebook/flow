// @flow

import type T from "./empty_file";

export default class {
  // $FlowExpectedError[value-as-type]
  p: T;
}

import x from './empty_non_flow_file';

export type S = {
  errors:
    // $FlowExpectedError[value-as-type]
    | x.a
    // $FlowExpectedError[value-as-type]
    | x.b<>
    // $FlowExpectedError[value-as-type]
    | x.c<mixed>
};
