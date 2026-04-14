import type RecordModel from './RecordModel';

export function validateRecord(record: RecordModel): { +valid: boolean } {
  return {valid: record.get('count') >= 0};
}
