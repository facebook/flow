// @flow

import * as React from 'react';

type Props = {foo: string, bar?: number};

/* ========================================================================== *\
 * Correct usage                                                              *
\* ========================================================================== */

const MemoString = React.memo('pretty useless');
<MemoString />;

const MemoNoProps = React.memo(() => <span />);
<MemoNoProps />;

const MemoProps = React.memo(
  (props: Props): React.Node => <span title={props.foo}>{props.bar}</span>,
);
<MemoProps foo="foo" />;
<MemoProps foo="foo" bar={42} />;

const MemoClass = React.memo(
  class extends React.Component<Props> {
    render(): React.Node {
      return <span title={this.props.foo}>{this.props.bar}</span>;
    }
  },
);
<MemoClass foo="foo" />;
<MemoClass foo="foo" bar={42} />;

const MemoPropsCompare = React.memo(
  (props: Props): React.Node => <span title={props.foo}>{props.bar}</span>,
  (oldProps: Props, newProps: Props): boolean => oldProps.foo === newProps.foo,
);
<MemoPropsCompare foo="foo" bar={42} />;

/* ========================================================================== *\
 * Incorrect usage                                                            *
\* ========================================================================== */

<MemoProps />; // foo is missing
<MemoProps foo="foo" bar="not a number" />;
<MemoClass foo="foo" bar="not a number" />;
<MemoPropsCompare foo="foo" bar="not a number" />;

const MemoElement = React.memo(<span />);
const MemoNumber = React.memo(42);
