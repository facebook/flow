declare function f<X>(
  props: $Diff<
    {f: {extra: $ReadOnly<{...} & X>, ...}, ...},
    {f: {extra: $ReadOnly<{...} & X>, ...}, ...},
  >,
): void;

f({});
