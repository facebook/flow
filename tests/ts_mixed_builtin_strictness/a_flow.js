declare class FlowAnimal {}
declare class FlowDog extends FlowAnimal {
  bark(): void;
}

declare class FlowBuiltinBox<T> {
  value: T;
}
