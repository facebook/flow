const React = require('react');

type Props = {
    onKeyDown?: ?(e: SyntheticKeyboardEvent<>) => unknown,
}
class C1 extends React.Component<Props> {};
function _onKeyDown(e: SyntheticKeyboardEvent<C1>): void {};
<C1 onKeyDown={_onKeyDown} />;
