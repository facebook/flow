declare const sharedArrayBuffer: SharedArrayBuffer;

const lengthBacked = new Uint8Array(8);
lengthBacked as Uint8Array<ArrayBuffer>;
lengthBacked.buffer as ArrayBuffer;

const sharedConstructed = new Uint8Array(sharedArrayBuffer);
sharedConstructed as Uint8Array<SharedArrayBuffer>;
sharedConstructed.buffer as SharedArrayBuffer;

const filteredShared = sharedConstructed.filter(value => value > 0);
filteredShared as Uint8Array<ArrayBuffer>;
filteredShared.buffer as ArrayBuffer;

const mappedShared = sharedConstructed.map(value => value + 1);
mappedShared as Uint8Array<ArrayBuffer>;
mappedShared.buffer as ArrayBuffer;

const slicedShared = sharedConstructed.slice();
slicedShared as Uint8Array<ArrayBuffer>;
slicedShared.buffer as ArrayBuffer;

const subarrayOfShared = sharedConstructed.subarray();
subarrayOfShared as Uint8Array<SharedArrayBuffer>;
subarrayOfShared.buffer as SharedArrayBuffer;

const mappedSharedBigInt = new BigInt64Array(sharedArrayBuffer).map(value => value);
mappedSharedBigInt as BigInt64Array<ArrayBuffer>;
mappedSharedBigInt.buffer as ArrayBuffer;
mappedSharedBigInt.reduceRight((left, right) => left + right) as bigint;

const copiedFromShared = new Uint8Array(sharedConstructed);
copiedFromShared as Uint8Array<ArrayBuffer>;
copiedFromShared.buffer as ArrayBuffer;

const fromArray = Uint8Array.from([1, 2, 3]);
fromArray as Uint8Array<ArrayBuffer>;
fromArray.buffer as ArrayBuffer;

const ofArray = Uint8Array.of(1, 2, 3);
ofArray as Uint8Array<ArrayBuffer>;
ofArray.buffer as ArrayBuffer;

new Int8Array(1) as Int8Array<ArrayBuffer>;
new Uint8ClampedArray(1) as Uint8ClampedArray<ArrayBuffer>;
new Int16Array(1) as Int16Array<ArrayBuffer>;
new Uint16Array(1) as Uint16Array<ArrayBuffer>;
new Int32Array(1) as Int32Array<ArrayBuffer>;
new Uint32Array(1) as Uint32Array<ArrayBuffer>;
new Float16Array(1) as Float16Array<ArrayBuffer>;
new Float32Array(1) as Float32Array<ArrayBuffer>;
new Float64Array(1) as Float64Array<ArrayBuffer>;
new BigInt64Array(1) as BigInt64Array<ArrayBuffer>;
new BigUint64Array(1) as BigUint64Array<ArrayBuffer>;

new Float32Array(sharedArrayBuffer) as Float32Array<SharedArrayBuffer>;
new BigInt64Array(sharedArrayBuffer) as BigInt64Array<SharedArrayBuffer>;

declare const defaultTypedArray: Uint8Array;
defaultTypedArray as Uint8Array<ArrayBufferLike>;

declare const broadTypedArray: Uint8Array<ArrayBufferLike>;
broadTypedArray as Uint8Array;

declare const defaultInternalTypedArray: $TypedArray;
defaultInternalTypedArray as $TypedArray<ArrayBufferLike>;

declare const internalNumberShared: $TypedArrayNumber<SharedArrayBuffer>;
internalNumberShared.map(value => value) as $TypedArrayNumber<ArrayBuffer>;

declare const defaultArrayBufferView: ArrayBufferView;
defaultArrayBufferView as ArrayBufferView<ArrayBufferLike>;

declare const defaultInternalArrayBufferView: $ArrayBufferView;
defaultInternalArrayBufferView as $ArrayBufferView<ArrayBufferLike>;

declare const defaultDataView: DataView;
defaultDataView as DataView<ArrayBufferLike>;

declare const arrayBacked: Uint8Array<ArrayBuffer>;
arrayBacked.buffer as ArrayBuffer;
arrayBacked.buffer as SharedArrayBuffer; // ERROR

declare const sharedBacked: Uint8Array<SharedArrayBuffer>;
sharedBacked.buffer as SharedArrayBuffer;
sharedBacked.buffer as ArrayBuffer; // ERROR

sharedBacked as ArrayBufferView<SharedArrayBuffer>;
sharedBacked as ArrayBufferView<ArrayBufferLike>;

const dataView = new DataView(sharedArrayBuffer);
dataView as DataView<SharedArrayBuffer>;
dataView as ArrayBufferView<SharedArrayBuffer>;
dataView.buffer as SharedArrayBuffer;

declare const arrayOnlyView: ArrayBufferView<ArrayBuffer>;
arrayOnlyView as ArrayBufferView<ArrayBufferLike>;
arrayOnlyView as ArrayBufferView<SharedArrayBuffer>; // ERROR

type TypeScriptDOMShapes =
  | ArrayBufferLike
  | ArrayBufferView<ArrayBufferLike>
  | ArrayBufferView<ArrayBuffer>
  | Int8Array<ArrayBuffer>
  | Uint8Array<ArrayBuffer>
  | Uint8ClampedArray<ArrayBuffer>
  | Int16Array<ArrayBuffer>
  | Uint16Array<ArrayBuffer>
  | Int32Array<ArrayBuffer>
  | Uint32Array<ArrayBuffer>
  | Float16Array<ArrayBuffer>
  | Float32Array<ArrayBuffer>
  | Float64Array<ArrayBuffer>
  | BigInt64Array<ArrayBuffer>
  | BigUint64Array<ArrayBuffer>
  | DataView<ArrayBuffer>;

declare const domShape: TypeScriptDOMShapes;
domShape as TypeScriptDOMShapes;
