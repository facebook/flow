// @flow

export declare class FlowAnimal {}
export declare class FlowDog extends FlowAnimal {
  bark(): void;
}

export type FlowWideValue = {readonly value: FlowAnimal};
export type FlowNarrowValue = {value: FlowDog, extra: string};

export declare const flowNarrowValue: FlowNarrowValue;
