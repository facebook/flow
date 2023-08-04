//@flow
import {Foo} from './cycle1';

export const Bar: typeof Foo = Foo;
