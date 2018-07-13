//@flow
declare opaque type ID: string;

const x: ID = ('0': any)

// Strings are ok!
const data: {[ID]: number} = {
  [x]: 3,
}

declare opaque type ID2: number;

const y: ID2 = ('0': any)

// Numbers are ok!
const data2: {[ID2]: number} = {
  [y]: 3,
}

declare opaque type BadID: {a: string};

const z: BadID = ('0': any)

// Objects are not ok!
const dataBad: {[BadID]: number} = {
  [z]: 3,
}
