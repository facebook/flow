//@flow

type Props = {| foo: number |}
function Component(props: Props): React$Node { return null }
function Component2(props: {| bar: number |}): React$Node { return null }

const x: React$Element<typeof Component> = <Component foo={3} />; // Ok
const y = <Component bar={3} />; // Error missing foo, got bar
const z: React$Element<typeof Component2> = <Component bar={3} />; // Error, missing foo got bar

const fragment1 = <></>;
const fragment2 = <><Component foo={3}/></>;
const fragment3 = <><></></>;
const fragment4 = <><Component2 foo={3}/></>; // Error requires bar not foo
