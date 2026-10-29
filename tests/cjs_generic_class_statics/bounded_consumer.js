// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import Bounded, {id} from './bounded';
id(1) as number;
Bounded.id(1) as number;
id('x'); // OK - `T` in a static member is `any`
Bounded.id('x'); // OK - `T` in a static member is `any`
