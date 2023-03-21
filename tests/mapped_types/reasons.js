// We make a SingeltonStringT to implement mapped types via substitution.
// Let's make sure they have good reasons for error messages.


// Make sure the substituted SingletonStringTs have good reasons
{
  type TakesFoo<T: 'foo'> = T;

  type FooBarObj = {foo: number, bar: number};
  // TODO(jmbrown): Slices do not store property key locations right now. We
  // need those in order to improve this error message.
  type MappedFooBarObj = {[key in keyof FooBarObj]: TakesFoo<key>}; // ERROR

  ({foo: 'foo', bar: 'bar'}: MappedFooBarObj);
}
