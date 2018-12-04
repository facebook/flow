//@flow
const React = require('react');

type Props = {| foo: number |};
const FancyButton = React.forwardRef<Props, _>((props, ref) => (
  <button ref={ref} className="FancyButton">
  </button>
));

(FancyButton: React.AbstractComponent<Props, void, HTMLButtonElement>);

const _a = <FancyButton />; // Error, missing foo
const _b = <FancyButton foo={3} />;
const _c = <FancyButton foo={3} bar={3} />; // Error bar, not allowed in exact props

const goodRef = React.createRef<HTMLButtonElement>();
const _d = <FancyButton foo={3} ref={goodRef} />;

const badRef = React.createRef<HTMLDivElement>();
const _e = <FancyButton foo={3} ref={badRef} />; // Incorrect ref type

const _f =  <FancyButton foo={3} ref={x => x} />;
const _g =  <FancyButton foo={3} ref={(x: null | HTMLDivElement) => x} />; // Incorrect ref type
