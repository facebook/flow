The `Config` type describes settings that may be only partially present: the whole config can be missing, and the `database` and `replica` levels are each optional.

Define a type alias `ReplicaSettings` for the type you get by reaching `database` then `replica`, accounting for each level possibly being absent. Then write `connectionString(replica: ReplicaSettings): string` that returns `` `${host}:${port}` `` when the replica settings are present and `'primary'` when they are not.

Build `ReplicaSettings` by reaching through the chain rather than restating the host/port shape.
