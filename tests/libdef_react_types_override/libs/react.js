declare type React$Node =  // intentional-libdef-override
  | void
  | null
  | boolean
  | number
  | string
  | React$Element<any>
  | Iterable<?React$Node>;
