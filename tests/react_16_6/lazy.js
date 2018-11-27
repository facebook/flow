//@flow

const React = require('react');

type Props = {| foo: number |};
function FunctionComponent(x: Props): React.Node { return null }
class ClassComponent extends React.Component<Props> {}

const LazyFunctionComponent = React.lazy( () => FunctionComponent );

const _a = <LazyFunctionComponent foo={3} />;
const _b = <LazyFunctionComponent />; // Error missing foo
const _c = <LazyFunctionComponent foo={3} bar={3} />; // Error extra bar
const _d = <LazyFunctionComponent foo="string" />; // Error wrong type for foo

const LazyClassComponent = React.lazy( () => ClassComponent );

const _e = <LazyClassComponent foo={3} />;
const _f = <LazyClassComponent />; // Error missing foo
const _g = <LazyClassComponent foo={3} bar={3} />; // Error extra bar
const _h = <LazyClassComponent foo="string" />; // Error wrong type for foo
