// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import Box, {id, count, echo} from './box';
const numberResult: number = id(1);
const stringResult: string = id('x');
const defaultNumber: number = Box.id(1);
const defaultString: string = Box.id('x');
count as number;
echo(true) as boolean;
count as string; // ERROR
echo(true) as string; // ERROR
