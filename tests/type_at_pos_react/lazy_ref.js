//@flow

const React = require('react');
const {useImperativeHandle} = React;

function Demo(props: empty, ref: {current: {|moo(x: string): void|} | null} | ({|moo(x: string): void|} | null) => mixed) {
  useImperativeHandle(ref, () => ({
    moo(x: string) {},
  }));
  return null;
}

const Lazy1 = React.lazy(async () => {
  const x = { default: React.forwardRef(Demo) };
  return x;
});

const Lazy2 = React.lazy(async () => {
  const x = import('./exports-component');
  return x;
});
