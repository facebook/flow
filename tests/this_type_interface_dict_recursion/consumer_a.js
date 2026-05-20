import type {Zippable, ZippableFile} from './test';

declare const x: ZippableFile | Zippable;
const _y: Zippable | ZippableFile = x;
