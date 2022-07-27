// @flow

declare type React$Node =
  | void
  | null
  | boolean
  | number
  | string
  | React$Element<any>
  | Iterable<?React$Node>;
