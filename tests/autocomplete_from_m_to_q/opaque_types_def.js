// @flow

export opaque type RemoteOpaque = { foo: string, bar: number };
export opaque type RemoteObject : { foo: string, ... } = { foo: string, bar: number }
