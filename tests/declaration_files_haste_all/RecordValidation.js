import type RecordModel from './RecordModel';

export function validateRecord(record: RecordModel): { readonly valid: boolean } {
  return {valid: record.get('count') >= 0};
}
