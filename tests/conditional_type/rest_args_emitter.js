export class Emitter<Events extends {[string]: Array<unknown>}> {
  emit<E extends keyof Events, A extends Events[E]>(event: E, ...args: A): void {}
}
