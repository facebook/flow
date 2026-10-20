declare const implementation: (s: string) => number;
type Local = number;
declare namespace API {
  export { implementation as call, type Local as Item };
  export type { Local as OtherItem };
  export const value: number;
  export let mutable: string;
  export var variable: boolean;
  export class Member { field: number; method(s: string): number; }
  export abstract class Base { abstract method(): void; }
  export function helper(s: string): number;
}
