flowDogTsBox as TsBuiltinBox<FlowAnimal>; // OK: TypeScript generic with Flow arguments.
tsDogFlowBox as FlowBuiltinBox<TsAnimal>; // ERROR: Flow generic with TypeScript arguments.

// The key set follows the interface's own file, not this one: TypeScript `keyof`
// sees inherited members, Flow `$Keys` sees only own ones.
'custom' as typeof tsKeys; // OK: own member.
'mousedown' as typeof tsKeys; // OK: inherited through `extends`.
'custom' as typeof flowKeys; // OK: own member.
'mousedown' as typeof flowKeys; // ERROR: inherited members are not Flow keys.

// An inherited key still selects the keyed overload rather than the `string` fallback.
tsListener.on('mousedown', 1); // OK
tsListener.on('custom', 1); // ERROR: the keyed overload types `custom` as `string`.
