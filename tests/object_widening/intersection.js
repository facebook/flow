//@flow
type Props = {foo: number};
type OtherProps = {bar: number};
type Intersected = Props & OtherProps;
const i: Intersected = {...null}; // error, missing foo and bar
