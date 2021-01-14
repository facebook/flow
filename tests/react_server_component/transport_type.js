//@flow
import type {TransportValue} from 'react';
declare var inexactObject: {...};
declare var fn: () => void;
declare var hasInexactObj: {| foo: {| foo: {| foo: {| foo: {...} |}|}|} |};
declare var numberIndexer: {[string]: number};
declare var hasManyCompatibleValues: {|
  foo: number,
  bar: ?React$Element<any>,
  baz?: number,
|}
declare var hasOneIncompatibleValue: {|
  foo: number,
  bar: ?React$Element<any>,
  baz?: number,
  qux: () => any,
|}

(inexactObject: TransportValue); // Error
(fn: TransportValue); // Error
(hasInexactObj: TransportValue); // Error
(numberIndexer: TransportValue); // Ok
(hasManyCompatibleValues: TransportValue); // Ok
(hasOneIncompatibleValue: TransportValue); //Error
(undefined: TransportValue); // Ok
