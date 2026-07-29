flowDogTsBox as TsBuiltinBox<FlowAnimal>; // OK: TypeScript generic with Flow arguments.
tsDogFlowBox as FlowBuiltinBox<TsAnimal>; // ERROR: Flow generic with TypeScript arguments.
