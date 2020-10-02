---
layout: guide
---
The coverage command provides a metric of the amount of checking that Flow has
performed on each part of your code. A program with high Flow coverage should
increase your confidence that Flow has detected any potential runtime errors.

The determining factor for this is the presence of [`any`](../types/any/) in the
inferred type of each expression. An expression whose inferred type is `any` is
considered *uncovered*, otherwise it is considered *covered*.

To see why this metric was chosen for determining Flow's effectiveness, consider
the example
```js
const one: any = 1;
one();
```
This code leads to a runtime type error, since we are attempting to perform a call
on a number. Flow, however, does not flag an error here, because we have annotated
variable `one` as `any`. Flow's checking is effectively turned off whenever `any`
is involved, so it will silently allow the call. The use of this *unsafe* type has
rendered the type checker ineffective, and the coverage metric is here to surface
this, by reporting all instances of `one` as uncovered.

## Design Space <a class="toc" id="toc-design-space" href="#toc-design-space"></a>

**Which types should be "covered"?**

What was described above is a rather coarse grained way to determine coverage. One
could imagine a criterion that flags expressions as uncovered if *any* part of their
type includes `any`, for example `Array<any>`. While there is value in a metric like
this, the "uncovered" part of the type will typically be uncovered through various
operations on values of this type. For example, in the code
```js
declare var arr: Array<any>;
arr.forEach(x => {});
```
the parameter `x` will be flagged as uncovered. Also, in practice, a strict criterion
like this would be too noisy and rather expensive to compute on the fly.

**Union types**

An exception to this principle are union types: the type `number | any` is considered
*uncovered*, even though technically `any` is not the top-level constructor.
Unions merely encode an option among *a set of* other types. In that sense we are
conservatively viewing an expression as uncovered, when at least one possible type
of that expression causes limited checking. For example, in the code
```js
let x: number | any = 1;
x = "a";
```
Flow will let you assign anything to `x`, which reduces confidence in the use
of `x` as a number. Thus `x` is considered uncovered.

**The empty type**

An interesting type from a coverage perspective is the `empty` type. This type
roughly corresponds to *dead code*. As such checking around expressions with type
`empty` is more relaxed, but for a good reason: this code will not be executed at
runtime. Since it is a common practice to clean up such code, Flow coverage will
also report code whose type is inferred to be `empty`, but distinguishes it from
the case of `any`.


## Command Line Use <a class="toc" id="toc-command-line-use" href="#toc-command-line-use"></a>

To find out the coverage of a file foo.js with the following contents
```js
// @flow
function add(one: any, two: any): number {
  return one + two;
}

add(1, 2);
```
you can issue the following command
```
$ flow coverage file.js
Covered: 50.00% (5 of 10 expressions)
```
This output means that 5 out of the 10 nodes of this program were inferred to have type
`any`. To see exactly which parts are uncovered you can also pass one of the following
flags:
* `--color`: This will print foo.js on the terminal with the uncovered locations in
red color.
* `--json`: This will list out all location spans that are uncovered under
the tag `"uncovered_locs"`.

Finally, as an example of dead code, consider the code
```js
function untypedAdd(one, two) {
  return one + two;
}
```
Note that function `untypedAdd` is never called, so `one` and `two` will be inferred to have
type `empty`. In the colored version of this command these parts appear in blue color,
and in the JSON version they are under the tag `"empty_locs"`.

**Use on multiple files**

If you want to check coverage of multiple files at once, Flow offers the
`batch-coverage` command:
```
$ flow batch-coverage dir/
```
will report coverage statistics for each file under `dir/`, as well as aggregate
results.

Note that `batch-coverage` requires a non-lazy Flow server.
