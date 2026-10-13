declare namespace LibMergedMember {
  interface Options {
    a: number;
  }
  interface Thing {
    get(): string;
  }
  interface ThingConstructor {
    new (options?: Options): Thing;
  }
  var Thing: ThingConstructor;
  function make(): Thing;
}
