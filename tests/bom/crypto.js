/* @flow */

const x: string = crypto.randomUUID();

const i8: Int8Array = crypto.getRandomValues(new Int8Array(2));
const u8: Uint8Array = crypto.getRandomValues(new Uint8Array(2));
const uc8: Uint8ClampedArray = crypto.getRandomValues(new Uint8ClampedArray(2));
const i16: Int16Array = crypto.getRandomValues(new Int16Array(2));
const u16: Uint16Array = crypto.getRandomValues(new Uint16Array(2));
const i32: Int32Array = crypto.getRandomValues(new Int32Array(2));
const u32: Uint32Array = crypto.getRandomValues(new Uint32Array(2));
const i64: BigInt64Array = crypto.getRandomValues(new BigInt64Array(2));
const u64: BigUint64Array = crypto.getRandomValues(new BigUint64Array(2));

const f32: Float32Array = crypto.getRandomValues(new Float32Array(2)); // incorrect
const f64: Float64Array = crypto.getRandomValues(new Float64Array(2)); // incorrect
