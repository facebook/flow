//@flow
const React = require('react');
const Component = require('./component');

// This file does not end in .server.js or .hybrid.js, so we should be
// allowed to pass non-transport-value props
declare var inexactObject: {...};
<Component num={3} exactObject={{foo: 3}} inexactObject={inexactObject} fn={() => {}} />;
