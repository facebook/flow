/*
 * @providesModule G
 * @flow
 */

/**
 * This test verifies that $Flow$ModuleRef is properly covariant.
 * It's a real-life use case from WWW where a module ref is used
 * as a covariant property of JSResourceReference, which is a
 * lazy loading abstraction.
 */

type $unwrap = <T>(l: JSResourceReference<T>) => T;

class JSResourceReference<+T> {
  +_moduleId: $Flow$ModuleRef<T>;

  constructor(moduleId: $Flow$ModuleRef<T>) {
    this._moduleId = moduleId;
  }

  static loadAll<I: Array<JSResourceReference<mixed>>>(
    loaders: I,
    callback: (...modules: $TupleMap<I, $unwrap>) => void,
  ): void {
    // ...load the modules and then pass them to the callback
  }
}

JSResourceReference.loadAll(
  [new JSResourceReference('m#E'), new JSResourceReference('m#F')],
  (E, F) => {
    (E: string);
    (F: number);
    (E: number); // Error - E exports string
    (F: string); // Error - F exports number
  }
);
