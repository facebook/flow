/*
 * @flow
 * @jsx call()
 */

function mk() { return <div /> }; // Error, call can't extract base name

function call() { return 42; }
