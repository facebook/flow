// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import Bounded, {id} from './bounded';
id(1) as number;
Bounded.id(1) as number;
id('x'); // ERROR
Bounded.id('x'); // ERROR
