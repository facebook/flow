The file `source.ts` contains a TypeScript program that maintains a small
event bus. It registers a subscriber's handler as a listener and then publishes
a few events through the bus, expecting the subscriber to record each one.

Convert it to idiomatic Flow in `main.js` so that every published event is
recorded by the subscriber.
