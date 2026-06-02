export type AnySchema =
  | {
      readonly kind: 'base',
    }
  | {
      readonly kind: 'list',
    };
