// @flow

import * as React from "react";
declare opaque type A;
declare var props: { data: A, name: string };

const a = <div style={props.data} id="literal" hidden {...props} />;
const b = <div>{props.name}</div>;

declare var str: string;
const c = <div>{str}</div>;

class C extends React.Component<any> {}
const o = {
  r: C
};

const d = <o.r>blah</o.r>;
