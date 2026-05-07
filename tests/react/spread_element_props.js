import * as React from 'react';

export type Props = {f: any} | {g: any};

declare const Component: (props: Props) => React.Node;

declare const props: Readonly<{
    ...React.ElementConfig<typeof Component>,
}>;

const {...spreadProps} = props;
const _ = <Component {...spreadProps} />; // okay
