const React = require('react');

type Props = Readonly<{bar: string, baz: number}>;
declare const TestComponent: React.ComponentType<Props>;
const props: Props = {bar: '', baz: 0};

<TestComponent {...props} />; //breaks
({...props}) as Props; //works
