declare type React$Node =  // intentional-libdef-override
  | void
  | null
  | boolean
  | number
  | string
  | React.JSX.Element
  | Iterable<?React$Node>;
