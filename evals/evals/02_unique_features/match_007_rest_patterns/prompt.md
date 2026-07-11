Write two Flow functions using `match` expressions with rest patterns:

1. `tail(arr: [number, number, number, number]): [number, number, number]` — return all elements except the first using an array rest pattern.
2. `splitConfig(config: {host: string, port: number, debug: boolean, verbose: boolean})` — split into `{connection: {host, port}, flags: {debug, verbose}}` using an object rest pattern.
