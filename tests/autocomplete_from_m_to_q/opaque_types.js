// @flow

import type {RemoteOpaque, RemoteObject} from './opaque_types_def';

opaque type LocalOpaque = { foo: string, bar: number, ... };
opaque type LocalObject : { foo: string, ... } = { foo: string, bar: number, ... }

declare const a : RemoteOpaque;
declare const b : RemoteObject;
declare const c : LocalOpaque;
declare const d : LocalObject;

 a.f;
// ^

 b.f;
// ^

 c.f;
// ^

 d.f;
// ^
