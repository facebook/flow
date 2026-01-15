// @flow

export type MyType = {
  id: string,
  value: number,
};

export interface DataStore {
  get(key: string): MyType | null;
  set(key: string, value: MyType): void;
}
