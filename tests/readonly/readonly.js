/* @flow */

type NormalObject = {
  name: string
}

type ReadOnlyObject = $ReadOnly<{
  name: string
}>

type ReadOnlyList = $ReadOnly<Array<string>>

function acceptsReadOnlyObject(obj: ReadOnlyObject) {
  obj.name = 'bar'; // error

  Object.assign(obj, {
    name: 'baz'
  });
}

function acceptsObjectButDelcaresItFrozen(obj: $ReadOnly<NormalObject>) {
  obj.name = 'foo'; // error
}

function acceptsReadOnlyArray(arr: ReadOnlyList) {
  arr.push(''); // error
  arr.pop(); // error
  arr[0] = ''; // error
}

function makesFrozen(): ReadOnlyObject {
  return Object.freeze({name: 'bar'}); // ok
}

function makesNotFrozen(): ReadOnlyObject {
  return {name: 'bar'}; // ok
}

acceptsReadOnlyObject(Object.freeze({name: 'bar'})); // ok
acceptsReadOnlyObject({name: 'bar'}); // ok
