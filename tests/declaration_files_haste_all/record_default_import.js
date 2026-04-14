import type Immutable from 'immutable';
import type {FormProps} from './RecordFormTypes';

import RecordModel from './RecordModel';
import {validateRecord} from './RecordValidation';

declare const tags: Immutable.List<string>;

const record = new RecordModel({
  count: 1,
  label: 'ok',
  tags,
});

record.get('tags').get(0) as string | void;
record.tags.get(0) as string | void;

declare const formProps: FormProps;

formProps.params.get('tags').get(0) as string | void;
validateRecord(record).valid as boolean;
