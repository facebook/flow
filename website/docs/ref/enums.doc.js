/* @flow */
/*
---
id: enums
title: Enums
permalink: /docs/enums.html
prev: react.html
---
*/

/*
In Flow you can [represent enums using literal types](builtins.html#literal-types).
*/

type Month = 'jan' | 'feb' | 'mar' | 'apr' | 'jun' | 'jul' | 'aug' | 'sep' | 'oct' | 'nov' | 'dic'

const jan: Month = 'jan';
// $ExpectError
const wrong: Month = 'wrong'; // 'wrong' is not a Month

/*
This is very handy, but sometimes you need to access the enum definition at runtime (i.e. at a value level).

Suppose for example that you want to associate a value to each month of the previous example.

You could do
*/

const monthNumbers = {
  jan: 1,
  feb: 2,
  mar: 3,
  apr: 4,
  may: 5,
  jun: 6,
  jul: 7,
  aug: 8,
  sep: 9,
  oct: 10,
  nov: 11,
  dec: 12
};

function printMonthNumber(month: Month) {
  console.log(monthNumbers[month]);
}

printMonthNumber('aug'); // 8
printMonthNumber('foo'); // 'foo' is not a Month

/*
but this doesn't feel very DRY, as we had to explicitly define the months names twice.

In situations like this one, you can leverage the `$Keys<T>` operator. Let's see another enum example, this time using `$Keys`
*/

const weekdays = {
  mon: 1,
  tue: 2,
  wed: 3,
  thu: 4,
  fry: 5,
  sat: 6,
  sun: 7
};

type Weekday = $Keys<typeof weekdays>;

const wed: Weekday = 'wed';
// $ExpectError
const nope: Weekday = 'nope'; // 'nope' is not a Month

/*
In the example above, the type of `Weekday` is equivalent to `type Weekday = 'mon' | 'tue' | ... | 'sun'`, but Flow was able to extract it from the keys of `weekdays`.

### About `$Enum`
Flow also provides the `$Enum<T>` operator, which is an alias of `$Keys<T>`. However, you should stick to `$Keys` as `$Enum` will likely be removed in the future.
*/
