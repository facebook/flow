declare class FlowAnimal {}
declare class FlowDog extends FlowAnimal {
  bark(): void;
}

declare class FlowBuiltinBox<T> {
  value: T;
}

declare interface FlowBaseMap {
  mousedown: number;
}
declare interface FlowDerivedMap extends FlowBaseMap {
  custom: string;
}
declare var flowKeys: $Keys<FlowDerivedMap>;
