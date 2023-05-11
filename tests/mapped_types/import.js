import type {
  MappedO,
  AddOptional,
  AllReadonly,
  ParameterizedId,
  ParameterizedPartial,
  ParameterizedReadonly,
  MappedNonHomomorphic,
  SemiHomomorphic,
} from './export';

// No modifiers concrete
{
  ({foo: 3, bar: 'str', baz: true}: MappedO); // OK

  declare const mapped: MappedO;
  (mapped: {foo: number, bar?: string, +baz: bool}); // OK
  (mapped: {foo: empty, bar: empty, +baz: empty}); // ERROR
  mapped.baz = true; // ERROR
}

// Add optional concrete
{
  ({foo: undefined, bar: undefined, baz: undefined}: AddOptional); // OK
  declare const addOptional: AddOptional;
  (addOptional: {foo?: number, bar?: string, +baz?: bool}); // OK
  addOptional.baz = true; // ERROR
}

// Add readonly concrete
{
  ({foo: 3, bar: undefined, baz: true}: AllReadonly);
  declare const readonly: AllReadonly;
  readonly.foo = 4; // ERROR;
  readonly.bar = 'str'; // ERROR;
  readonly.baz = false; // ERROR;
}

// All of these tests are the same as above but use a parameterize type alias
type O = {foo: number, bar?: string, +baz: bool};
// No modifiers parameterized 
{
  ({foo: 3, bar: 'str', baz: true}: ParameterizedId<O>); // OK

  declare const mapped: ParameterizedId<O>;
  (mapped: {foo: number, bar?: string, +baz: bool}); // OK
  (mapped: {foo: empty, bar: empty, +baz: empty}); // ERROR
  mapped.baz = true; // ERROR
}

// Add optional parameterized 
{
  ({foo: undefined, bar: undefined, baz: undefined}: ParameterizedPartial<O>); // OK
  declare const addOptional: ParameterizedPartial<O>;
  (addOptional: {foo?: number, bar?: string, +baz?: bool}); // OK
  addOptional.baz = true; // ERROR
}

// Add readonly parameterized 
{
  ({foo: 3, bar: undefined, baz: true}: ParameterizedReadonly<O>);
  declare const readonly: ParameterizedReadonly<O>;
  readonly.foo = 4; // ERROR;
  readonly.bar = 'str'; // ERROR;
  readonly.baz = false; // ERROR;
}

// Non-homomorphic mapped types
{
  ({foo: 3, bar: 3}: MappedNonHomomorphic); // OK!
  ({foo: null, bar: null}: MappedNonHomomorphic); // ERROR!
}

// Semi-homomorphic mapped types
{
  declare const semi: SemiHomomorphic<{+foo: number, bar: string}, 'foo'>;
  (semi: {+foo: number}); // OK
  (semi: {foo: number}); // ERROR
}
