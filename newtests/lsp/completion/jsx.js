// @flow

var React = require('react');

type Props = {a :number}

class C extends React.Component<Props> {
}
function D(props: Props) {
}

  // <- space
<C   />;// <- space
<D   />// <- space
<C. />
C.
