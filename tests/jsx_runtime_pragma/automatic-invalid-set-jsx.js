/**
 * @flow
 * @jsxRuntime automatic
 * @jsx foo
 */

// TODO: When jsxRuntime is automatic, jsx pragma should be disallowed.
// But at least it shouldn't crash!

declare function foo(...any): void;

<hello />;
