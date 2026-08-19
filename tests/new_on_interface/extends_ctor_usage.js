import type {Bar} from './extends_ctor_lib';
import {FromStatics} from './extends_ctor_lib';

new FromStatics() as Bar; // OK: the construct signature's return type
FromStatics.of('x') as Bar; // OK: statics inherited from `StaticsCtor`
FromStatics.tag as string; // OK
FromStatics.nope; // ERROR
