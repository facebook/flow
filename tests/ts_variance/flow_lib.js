// @flow

export declare class FlowAnimal {}
export declare class FlowDog extends FlowAnimal {
  bark(): void;
}

export declare class FlowBox<T> {
  value: T;
}

export type FlowAnimalMethod = {
  cb(x: FlowAnimal): void,
};

export type FlowDogMethod = {
  cb(x: FlowDog): void,
};

export type FlowReadonlyValue = {readonly value: string};
export type FlowMutableValue = {value: string};

export type FlowWideValue = {value: FlowAnimal};
export type FlowNarrowValue = {value: FlowDog};

export type FlowPresentValue = {a: FlowAnimal};
export type FlowOptionalValues = {a?: FlowAnimal, b?: FlowAnimal};
