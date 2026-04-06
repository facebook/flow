declare abstract class ServerState {
  abstract [Symbol.iterator](): Iterator<any>;
  abstract [PRIVATE_INITIALIZE_SYMBOL](): Promise<void>;
}
