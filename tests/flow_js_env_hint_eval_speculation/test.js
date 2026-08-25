// @flow

import type {TraceData, TraceTimelineNode} from './types';

const traceData: ReadonlyMap<string, TraceData> = new Map([
  [
    't1',
    {
      nodes: new Map([
        [
          'n1',
          {
            label: 'one',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from(['n2']),
          },
        ],
        [
          'n2',
          {
            label: 'two',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from(['n3']),
          },
        ],
        [
          'n3',
          {
            label: 'three',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from(['n4']),
          },
        ],
        [
          'n4',
          {
            label: 'four',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from(['n5']),
          },
        ],
        [
          'n5',
          {
            label: 'five',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from(['n6']),
          },
        ],
        [
          'n6',
          {
            label: 'six',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from([]) as Array<string>,
          },
        ],
      ]) as ReadonlyMap<string, TraceTimelineNode>,
    },
  ],
  [
    't2',
    {
      nodes: new Map([
        [
          'n1',
          {
            label: 'one',
            ts: 0,
            metadata: new Map<string, string>(),
            parent_ids: Array.from([]) as Array<string>,
          },
        ],
      ]) as ReadonlyMap<string, TraceTimelineNode>,
    },
  ],
]);

traceData;
