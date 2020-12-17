const o1 = { x: 0, y: "" };
const o2 = { z: o1 }

const o3: { w: typeof o2 } = {};
o3.w = o2;

//declare module.exports: { w: any };

module.exports = o3;
