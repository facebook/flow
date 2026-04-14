import type {ListSchema} from './ListSchema';
import type {AnySchema as AnySchemaViaStar} from './schema_index';
import type {AnySchema as AnySchemaViaTypeStar} from './schema_index_type';

declare const schema: ListSchema;

schema as AnySchemaViaStar;
schema as AnySchemaViaTypeStar;
