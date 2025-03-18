const e = match (x) {
  | "s"
  | true
  | null => 0,
  {foo: | false
        | undefined} => 0,
};
