// @flow

import * as React from 'react';

import {
  type FlowWideValue,
  flowNarrowValue,
} from './object_slice_flow_base';
import {
  FlowComponent,
  type FlowTypeSpreadOfFlow,
  type FlowTypeSpreadOfTs,
  type MixedIntersectionValue,
  type SuperAccumulatedValue,
  flowValueSpreadOfFlow,
  flowValueSpreadOfTs,
} from './object_slice_flow_transforms';
import {
  type TsTypeSpreadOfFlow,
  tsValueSpreadOfFlow,
} from './object_slice_ts';

declare const mixedIntersectionValue: MixedIntersectionValue;
mixedIntersectionValue as FlowWideValue; // OK: intersection slices join strictness

declare const superAccumulatedValue: SuperAccumulatedValue;
superAccumulatedValue as FlowWideValue; // OK: superclass slices join strictness

<FlowComponent {...flowNarrowValue} />; // OK: config and default-props slices join strictness

declare const tsTypeSpreadOfFlow: TsTypeSpreadOfFlow;
tsTypeSpreadOfFlow as FlowWideValue; // OK: TypeScript target joins with Flow operand
tsValueSpreadOfFlow as FlowWideValue; // OK: TypeScript target joins with Flow operand

declare const flowTypeSpreadOfTs: FlowTypeSpreadOfTs;
flowTypeSpreadOfTs as FlowWideValue; // OK: Flow target joins with TypeScript operand
flowValueSpreadOfTs as FlowWideValue; // OK: Flow target joins with TypeScript operand

declare const flowTypeSpreadOfFlow: FlowTypeSpreadOfFlow;
flowTypeSpreadOfFlow as FlowWideValue; // ERROR: Flow type-spread target and operand
flowValueSpreadOfFlow as FlowWideValue; // ERROR: Flow value-spread target and operand
