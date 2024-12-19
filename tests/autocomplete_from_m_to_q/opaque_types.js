// @flow

import type {RemoteOpaque, RemoteObject} from './opaque_types_def';

opaque type LocalOpaque = { foo: string, bar: number };
opaque type LocalObject : { foo: string, ... } = { foo: string, bar: number }

declare var a : RemoteOpaque;
declare var b : RemoteObject;
declare var c : LocalOpaque;
declare var d : LocalObject;

 a.f;
// ^

 b.f;
// ^

 c.f;
// ^

 d.f;
// ^
