//@flow
const React = require('react');

type Props = {|+foo: number|};
const FancyButton = React.forwardRef<Props, ButtonInstance>((props, ref) => (
  <button ref={ref} className="FancyButton"></button>
));

FancyButton as component(ref: React.RefSetter<ButtonInstance>, ...Props)

const _a = <FancyButton />; // Error, missing foo
const _b = <FancyButton foo={3} />;
const _c = <FancyButton foo={3} bar={3} />; // Error bar, not allowed in exact props

const goodRef = React.createRef<ButtonInstance>();
const _d = <FancyButton foo={3} ref={goodRef} />;

const badRef = React.createRef<DivInstance>();
const _e = <FancyButton foo={3} ref={badRef} />; // Incorrect ref type

const _f = <FancyButton foo={3} ref={x => x} />;
const _g = <FancyButton foo={3} ref={(x: null | DivInstance) => x} />; // Incorrect ref type

type FooProps = {|foo: number|};

const UnionRef = React.forwardRef<
  FooProps,
  ButtonInstance | AInstance,
>((props: FooProps, ref): React.MixedElement => {
  if (props.foo === 0) {
    return <a ref={ref} />;
  }

  return <button ref={ref} />;
});

const unionRef = React.createRef<ButtonInstance | AInstance>();
const _h = <UnionRef foo={0} ref={unionRef} />;
const _i = <UnionRef foo={1} ref={unionRef} />;

const badUnionRef = React.createRef<ButtonInstance | DivInstance>();
const _j = <UnionRef foo={3} ref={badUnionRef} />; // Error bad ref
