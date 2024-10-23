//@flow

type Props = {+foo: number, ...};
function Component(x: Props): React$Node {
  return null;
}

Component as component(...Props);
Component as component(...{...}); // Error, missing foo
Component as component(ref: React.RefSetter<void>, bar: number, foo: number, ...{...}); //Ok, extra prop bar
Component as component(ref: React.RefSetter<number | void>, ...Props); // Ok
Component as component(ref: React.RefSetter<number>, ...Props); // Error

class NotAComponent {}

function NotAFunctionComponent(x: Props) {
  return NotAComponent; // Error, not a component
}

NotAFunctionComponent as component(...Props);

function SpecificRender(): number {
  return 3;
}
SpecificRender as component(...{...}); // OK, covariant type argument
