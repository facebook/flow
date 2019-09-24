type Arg<T> = T => void; // OK
type ArgNeg<-T> = T => void; // OK
type ArgPos<+T> = T => void; // Error: +T in negative position
type FlipArgNeg<-T> = (T => void) => void; // Error: -T in positive position
type FlipArgPos<+T> = (T => void) => void; // OK
type Ret<T> = () => T; // OK
type RetNeg<-T> = () => T; // Error: -T in positive position
type RetPos<+T> = () => T; // OK
type FlipRetNeg<-T> = (() => T) => void; // OK
type FlipRetPos<+T> = (() => T) => void; // Error: +T in negative position

function arg<T>(x: T): void {} // OK
function argNeg<-T>(x: T): void {} // OK
function argPos<+T>(x: T): void {} // Error: +T in negative position
function flipArgNeg<-T>(x: T => void): void {} // Error: -T in positive position
function flipArgPos<+T>(x: T => void): void {} // OK
function ret<T>(): T { return (null: any) } // OK
function retNeg<-T>(x: T): T { return x } // Error: -T in positive position
function retPos<+T>(): T { return (null: any) } // OK
function flipRetNeg<-T>(x: () => T): void {} // OK
function flipRetPos<+T>(x: () => T): void {} // Error: +T in negative position

const arg1 = <T>(x: T): void => {} // OK
const argNeg1 = <-T>(x: T): void => {} // OK
const argPos1 = <+T>(x: T): void => {} // Error: +T in negative position
const flipArgNeg1 = <-T>(x: T => void): void => {} // Error: -T in positive position
const flipArgPos1 = <+T>(x: T => void): void => {} // OK
const ret1 = <T>(): T => (null: any) // OK
const retNeg1 = <-T>(x: T): T => x // Error: -T in positive position
const retPos1 = <+T>(): T => (null: any) // OK
const flipRetNeg1 = <-T>(x: () => T): void => {} // OK
const flipRetPos1 = <+T>(x: () => T): void => {} // Error: +T in negative position

const obj = {
  arg<T>(x: T): void {}, // OK
  argNeg<-T>(x: T): void {}, // OK
  argPos<+T>(x: T): void {}, // Error: +T in negative position
  flipArgNeg<-T>(x: T => void): void {}, // Error: -T in positive position
  flipArgPos<+T>(x: T => void): void {}, // OK
  ret<T>(): T { return (null: any) }, // OK
  retNeg<-T>(x: T): T { return x }, // Error: -T in positive position
  retPos<+T>(): T { return (null: any) }, // OK
  flipRetNeg<-T>(x: () => T): void {}, // OK
  flipRetPos<+T>(x: () => T): void {} // Error: +T in negative position
}
