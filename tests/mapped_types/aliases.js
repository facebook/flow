// Mapped Type aliases should not participate in the buggy TypeAppT ~> TypeAppT special case
type ReadOnly<O> = {+[key in keyof O]: O[key]};
type ReadOnlyDeeper<O> = ReadOnly<O>;
type ReadOnlyDeepest<O> = ReadOnlyDeeper<O>;

type O1 = {foo: number};
type O2 = {foo: string | number};

declare const x: ReadOnly<O1>;
(x: ReadOnly<O2>); // OK

declare const y: ReadOnlyDeeper<O1>;
(y: ReadOnlyDeeper<O2>); // OK

declare const z: ReadOnlyDeepest<O1>;
(z: ReadOnlyDeepest<O2>); // OK
