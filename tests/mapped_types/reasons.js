// We make a SingletonStringT to implement mapped types via substitution.
// Let's make sure they have good reasons for error messages.


// Make sure the substituted SingletonStringTs have good reasons
{
  type TakesFoo<T: 'foo'> = T;

  type FooBarObj = {foo: number, bar: number};
  type MappedFooBarObj = {[key in keyof FooBarObj]: TakesFoo<key>}; // ERROR

  ({foo: 'foo', bar: 'bar'}: MappedFooBarObj);
}
