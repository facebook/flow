/* @flow */

import React from 'react';

type DefaultProps = {
    foo: number
}

class OnlyGetter extends React.Component {
    static get defaultProps(): DefaultProps {
        return {
            foo: 3
        }
    }
}


class GetterAndSetter extends React.Component {
    static set defaultProps(props: DefaultProps) {
        // do nothing
    }

    static get defaultProps(): DefaultProps {
        return {
            foo: 3
        }
    }
}


class StaticDefaultProps extends React.Component {
    static defaultProps: DefaultProps = {
        foo: 3
    }
}

class LaterDefaultProps extends React.Component {
    static defaultProps: DefaultProps
}

LaterDefaultProps.defaultProps = {
    foo: 3
}