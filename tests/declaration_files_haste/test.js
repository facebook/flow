/* @flow */

var Implicit = require('ImplicitProvidesModule');
Implicit.fun() as string;

// Flow.js and Flow.js.flow are both @flow
var Flow = require('Flow');
Flow.x as empty; // error: should be number

// FlowShadowed.js is @flow, but FlowShadowed.js.flow is not, so it actually is NOT @flow.
var FlowShadowed = require('FlowShadowed');
FlowShadowed as empty; // no error, whole module is `any`

// Neither NotFlow.js nor NotFlow.js.flow are @flow, so this is `any`
var NotFlow = require('NotFlow');
NotFlow as empty; // no error, whole module is `any`

// NotFlowShadowed.js is not @flow, but NotFlowShadowed.js.flow is, so it actually is @flow.
var NotFlowShadowed = require('NotFlowShadowed');
NotFlowShadowed.x as empty; // error: should be number
