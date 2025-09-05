declare type React$Node =  // intentional-libdef-override
  | void
  | null
  | boolean
  | number
  | string
  | ExactReactElement_DEPRECATED<any>
  | Iterable<?React$Node>;
