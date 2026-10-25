export const localReceiver: Receiver = receiver;

const identity = localReceiver.identity();
identity as Receiver;
identity as string; // ERROR
identity.value as number;
localReceiver.requiresString(); // ERROR
localReceiver.defaults() as 1;
localReceiver.defaults() as string; // ERROR
const zero = localReceiver.defaults(0);
zero as 0;
zero as 1; // ERROR
const widened = localReceiver.widen(0);
widened as number;
widened as 0; // ERROR
