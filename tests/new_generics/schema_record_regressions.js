import {acceptSchema, ListSchema, StringSchema} from './schema_library';

const schema = ListSchema.build({
  items: StringSchema.build({}),
});

acceptSchema(schema) as ReadonlyArray<string> | null;

declare class RecordInstance<T: interface {}> {
  set<K: $Keys<T>>(key: K, value: T[K]): this & T;
}

type RecordOf<T: interface {}> = RecordInstance<T> & T;
type RecordFactory<TInput: interface {}, TRecord: interface {}> = (
  value: TInput,
) => TRecord;

declare class IndexedMap<K, +V> {
  get(key: K): V | void;
  map<U>(mapper: (value: V) => U): IndexedMap<K, U>;
}

declare class ValueSet<T> {}

declare const Immutable: {
  Map: <V>(obj: {[key: string]: V, ...}) => IndexedMap<string, V>,
  Record: <T: interface {}>(spec: T) => RecordFactory<T, RecordOf<T>>,
  Set: <T>(iter: Iterable<T>) => ValueSet<T>,
};

type GraphNodeInfo = {
  children: Array<number>,
  id: number,
 ...};

type GraphNodeRecord = RecordOf<GraphNodeInfo>;

const GraphNodeRecordFactory: RecordFactory<GraphNodeInfo, GraphNodeRecord> =
  Immutable.Record({
    children: [] as Array<number>,
    id: 0,
  });

declare const graphNodes: {[key: string]: GraphNodeInfo, ...};
declare const currentId: string;

const mappedGraphNodes = Immutable.Map(graphNodes).map(value =>
  GraphNodeRecordFactory(value).set('children', value.children),
);

const nodeForCurrentId = mappedGraphNodes.get(currentId);

if (nodeForCurrentId != null) {
  const children: ValueSet<number> = Immutable.Set(nodeForCurrentId.children);
}
