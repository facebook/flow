//@flow

const React = require('react');
const {useImperativeHandle} = React;

type T = {moo(x: string): void}
function Demo(props: {}, ref: ?({current: (T | null), ...} | ((inst: (T | null)) => mixed))) {
  useImperativeHandle(ref, () => ({
    moo(x: string) {},
  }));
  return null;
}

const Memo = React.memo(React.forwardRef(Demo));

function App() {
  // Error below: moo expects a string, given a number
  return <Memo ref={ref => ref && ref.moo(0)} />;
}
