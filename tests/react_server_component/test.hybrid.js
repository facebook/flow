//@flow
const React = require('react');
const Component = require('./component');
const IndexedComponent = require('./indexedComponent');

// This file does end in .server.js or .hybrid.js, so we should be
// NOT allowed to pass non-transport-value props
declare var inexactObject: {...};
declare var indexedObjectBad: {[string]: () => void};
declare var indexedObjectOk: {[string]: number};
<Component num={3} exactObject={{foo: 3}} inexactObject={inexactObject} fn={() => {}} />; // Error
<Component num={3} exactObject={{foo: 3}} />; // Ok
<Component {...inexactObject} num={3} exactObject={{foo: 3}} />; // Error, inexact may have unknown prop
<IndexedComponent {...indexedObjectBad} num={3} exactObject={{foo: 3}} />; // Error
<IndexedComponent {...indexedObjectOk} num={3} exactObject={{foo: 3}} />; // Ok

type InexactProps = {foo: number};
function InexactComponent(props: InexactProps): React.Node { return null }
<InexactComponent foo={3} bar={() => {}} />; // Inexact Props type is ok, but function passed is not
<InexactComponent {...inexactObject} foo={3} />; // Error
