// @flow

import React from 'React';

// Utility. We want a value for any.
const any: any = null;

/* ========================================================================== *\
 * Component Definitions                                                      *
\* ========================================================================== */

type Props_NoProps = {};

const Legacy_NoProps = React.createClass({propTypes: {}});
class Class_NoProps extends React.Component<void, Props_NoProps, void> {}
class ClassExact_NoProps
  extends React.Component<void, $Exact<Props_NoProps>, void> {}
class ClassPure_NoProps
  extends React.PureComponent<void, Props_NoProps, void> {}
const Function_NoProps = (props: Props_NoProps) => any;
const FunctionExact_NoProps = (props: $Exact<Props_NoProps>) => any;

const NoProps:
  | typeof Legacy_NoProps
  | typeof Class_NoProps
  | typeof ClassExact_NoProps
  | typeof ClassPure_NoProps
  | typeof Function_NoProps
  | typeof FunctionExact_NoProps
  = any;

type Props_ManyProps = {
  string1: string,
  string2: string,
  boolean1: boolean,
  boolean2: boolean,
  number: number,
};

const Legacy_ManyProps = React.createClass({
  propTypes: {
    string1: React.PropTypes.string.isRequired,
    string2: React.PropTypes.string.isRequired,
    boolean1: React.PropTypes.bool.isRequired,
    boolean2: React.PropTypes.bool.isRequired,
    number: React.PropTypes.number.isRequired,
  },
});
class Class_ManyProps extends React.Component<void, Props_ManyProps, void> {}
class ClassExact_ManyProps
  extends React.Component<void, $Exact<Props_ManyProps>, void> {}
class ClassPure_ManyProps
  extends React.PureComponent<void, Props_ManyProps, void> {}
const Function_ManyProps = (props: Props_ManyProps) => any;
const FunctionExact_ManyProps = (props: $Exact<Props_ManyProps>) => any;

const ManyProps:
  | typeof Legacy_ManyProps
  | typeof Class_ManyProps
  | typeof ClassExact_ManyProps
  | typeof ClassPure_ManyProps
  | typeof Function_ManyProps
  | typeof FunctionExact_ManyProps
  = any;

type Props_OptionalProps = {foo: ?number, bar?: number};

const Legacy_OptionalProps = React.createClass({
  propTypes: {
    foo: React.PropTypes.any.isRequired,
    bar: React.PropTypes.number,
  },
});
class Class_OptionalProps
  extends React.Component<void, Props_OptionalProps, void> {}
class ClassExact_OptionalProps
  extends React.Component<void, $Exact<Props_OptionalProps>, void> {}
class ClassPure_OptionalProps
  extends React.PureComponent<void, Props_OptionalProps, void> {}
const Function_OptionalProps = (props: Props_OptionalProps) => any;
const FunctionExact_OptionalProps = (props: $Exact<Props_OptionalProps>) => any;

const OptionalProps:
  | typeof Legacy_OptionalProps
  | typeof Class_OptionalProps
  | typeof ClassExact_OptionalProps
  | typeof ClassPure_OptionalProps
  | typeof Function_OptionalProps
  | typeof FunctionExact_OptionalProps
  = any;

type Props_DefaultProps = {
  foo: number,
  bar: number,
};

const Legacy_DefaultProps = React.createClass({
  propTypes: {
    foo: React.PropTypes.number.isRequired,
    bar: React.PropTypes.number.isRequired,
  },
  defaultProps: {
    foo: 42,
  },
});
class Class_DefaultProps extends React.Component<{foo: 42}, Props_DefaultProps, void> {
  static defaultProps = {
    foo: 42,
  };
}
class ClassExact_DefaultProps
  extends React.Component<{|foo: 42|}, $Exact<Props_DefaultProps>, void> {
  static defaultProps = {
    foo: 42,
  };
}
class ClassPure_DefaultProps
  extends React.PureComponent<{foo: 42}, Props_DefaultProps, void> {
  static defaultProps = {
    foo: 42,
  };
}
function Function_DefaultProps(props: Props_DefaultProps) {
  return any;
}
Function_DefaultProps.defaultProps = {
  foo: 42,
};
function FunctionExact_DefaultProps(props: $Exact<Props_DefaultProps>) {
  return any;
}
FunctionExact_DefaultProps.defaultProps = {
  foo: 42,
};

const DefaultProps:
  | typeof Legacy_DefaultProps
  | typeof Class_DefaultProps
  | typeof ClassExact_DefaultProps
  | typeof ClassPure_DefaultProps
  | typeof Function_DefaultProps
  | typeof FunctionExact_DefaultProps
  = any;

/* ========================================================================== *\
 * Tests                                                                      *
\* ========================================================================== */

<NoProps />; // OK: There are no props.
<NoProps foo={42} />; // OK: Extra props are fine.

<ManyProps />; // Error: There are no props.
<ManyProps // OK: All props are defined.
  string1="foo"
  string2={'bar'}
  boolean1
  boolean2={false}
  number={42}
/>;
<ManyProps // OK: Other props are allowed.
  string1="foo"
  string2={'bar'}
  boolean1
  boolean2={false}
  number={42}
  a={1}
  b={2}
  c={3}
/>;
<ManyProps // Error: All props have an incorrect type.
  string1={null}
  string2={null}
  boolean1={null}
  boolean2={null}
  number={null}
/>;
<ManyProps // OK: All props are defined.
  string1="foo"
  string2={'bar'}
  boolean1
  boolean2={false}
  {...{number: 42}}
/>;
<ManyProps // OK: All props are defined.
  {...{string1: 'foo', string2: 'bar'}}
  {...{boolean1: true, boolean2: false}}
  {...{number: 42}}
/>;
<ManyProps // Error: Missing `number`.
  {...{string1: 'foo', string2: 'bar'}}
  {...{boolean1: true, boolean2: false}}
/>;
<ManyProps // OK: Extra props are allowed. Error for exact types.
  string1="foo"
  string2={'bar'}
  boolean1
  boolean2={false}
  {...{number: 42, a: 1, b: 2, c: 3}}
/>;
<ManyProps // OK: `number` is overwritten at the end of the element.
  {...{string1: 'foo', string2: 'bar', number: (any: ?number)}}
  boolean1
  boolean2={false}
  number={42}
/>;
<ManyProps // Error: `number` cannot be null.
  boolean1
  boolean2={false}
  number={42}
  {...{string1: 'foo', string2: 'bar', number: (any: ?number)}}
/>;

<OptionalProps />; // Error: `foo` is required.
<OptionalProps foo={42} />; // OK: `foo` is defined.
<OptionalProps foo={undefined} />; // OK: `foo` is defined as undefined.
<OptionalProps // OK: Both props are defined with a correct type.
  foo={4}
  bar={2}
/>;
<OptionalProps // Error: `foo` has a bad type.
  foo="nope"
  bar={2}
/>;
<OptionalProps // Error: `bar` has a bad type.
  foo={4}
  bar="nope"
/>;

<DefaultProps // OK: It has all the props.
  foo={1}
  bar={2}
/>;
<DefaultProps // OK: It is missing a default prop.
  bar={2}
/>;
<DefaultProps // Error: It is missing a required non-default prop.
  foo={1}
/>;
