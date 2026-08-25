// @flow

export type TraceTimelineNode = Readonly<{
  label: string,
  metadata: ReadonlyMap<string, string>,
  parent_ids: ReadonlyArray<string>,
  ts: number,
}>;

export type TraceData = Readonly<{
  nodes: ReadonlyMap<string, TraceTimelineNode>,
}>;
