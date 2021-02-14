//@flow
type OtherProps = {| foo: number |};

declare function HOC<OwnProps: {}>(
    Component: ({|...OwnProps, ...OtherProps|}) => mixed,
): OwnProps => mixed;

const x = HOC((x: {| foo: number, bar: number |}) => null);
