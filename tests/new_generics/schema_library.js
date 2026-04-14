export type Schema<TValue> = {
  +__value: TValue,
};

export type ListSchemaType<TItem> = Schema<ReadonlyArray<TItem>>;
export type StringSchemaType = Schema<string>;

export const StringSchema = {
  build(_: {| |}): StringSchemaType {
    return {__value: ''} as any;
  },
};

export const ListSchema = {
  build<TItem>(config: {| items: Schema<TItem> |}): ListSchemaType<TItem> {
    return {__value: []} as any;
  },
};

export function acceptSchema<TValue>(schema: Schema<TValue>): TValue | null {
  return schema.__value;
}
