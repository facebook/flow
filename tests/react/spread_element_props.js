import * as React from 'react';

export type Props = {f: any} | {g: any};

declare var Component: (props: Props) => React.Node;

declare var props: Readonly<{
    ...React.ElementConfig<typeof Component>,
}>;

const {...spreadProps} = props;
const _ = <Component {...spreadProps} />; // okay
