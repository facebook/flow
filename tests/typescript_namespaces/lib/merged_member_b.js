declare namespace LibMergedMember {
  interface Options {
    b?: string;
  }
  interface Thing {
    extra(): number;
  }
  type Kind = keyof Options;
}
