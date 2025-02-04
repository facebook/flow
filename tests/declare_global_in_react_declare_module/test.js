// $FlowExpectedError[internal-type]
2 as React$Node; // error: 2 ~> 1: The bad shadow wins over the real global.
'' as ReactTypes.MyReact; // error: '' ~> 'react'
ReactValue; // error: value-namespaces in declare global are completely ignored
willBeIgnored; // error: values in declare global are completely ignored

import type {T1, T2} from 'react'; // test that the normal exported types can reference names within declare global
'' as T1; // error: '' ~> 'react'
'' as T2 // error: '' ~> 3
