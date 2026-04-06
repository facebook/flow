// Type and value with same exported name should not conflict
export interface X {}
export let X: string;

// export default should not conflict with export type default
export default class Y {}
export type { Y as default };
