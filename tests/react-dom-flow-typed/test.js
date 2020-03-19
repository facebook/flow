//@flow

// Ensure react-dom flow-typed libdef allows access to SyntheticEvents and react-dom module
// $JSXIntrinsics live inside flow and the react-dom module lives inside flow-typed

class NotSyntheticEvent {}

var x: SyntheticEvent<> = new NotSyntheticEvent();

import * as ReactDOM from 'react-dom';
ReactDOM.render(null, null); // Error, null is incompatible
