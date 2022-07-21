//@flow

const React = require('react');

class PolyReactClass<T> extends React.Component<{}> {}

<PolyReactClass />;

function PolyFunctionComponent<T>(props: {foo: ({x: T} => mixed)}): React.Node { return null }

<PolyFunctionComponent foo={() => {}}/>;
