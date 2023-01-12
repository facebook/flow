---
title: Constrained Writes
slug: /lang/constrained-writes
---

As of Flow v0.186.0, unannotated variables will be inferred to have a precise type by their initializer or initial assignment, and all subsequent assignments to that variable will be constrained by this type. This page shows some examples of how Flow determines what type an unannotated variable is inferred to have.

**If you want a variable to have a different type than what Flow infers for it, you can always add a type annotation to the variable’s declaration. That will override everything discussed in this page!**

### Variables initialized at their declarations {#toc-variables-initialized-at-their-declarations}

The common case  for unannotated variables is very straightforward: when a variable is declared with an initializer that is not the literal `null`, that variable will from then on have the type of the initializer, and future writes to the variable will be constrained by that type [[example]](https://flow.org/try/#0JYWwDg9gTgLgBAKjgQwM5wEoFNkGN4BmUEIcA5FDvmQNwBQdMAnmFnAArFjoC8cA3gB84YLgC44IYAA8sAEziCAvvTpysuADbJKcAG4640iQDsAriABGWKPXVadbA1DhNTF67bUbtu5yK5UCU4IblVNLHhRCDkzfDg+AFlkGAALADpUAEdYAAppAEo4AGpXegB6crgAA2jY-Gq4VLQ4ZlYa8ysbaroI+ABhEkgTLBN4Plz+aLAlYMCingA+AThKGDMoEzgAHkWpriVt8uWlOkqawfAIEbHG5vQ2rGrsPBh0y+HRmAAVFixtkLcRY9PpwLAREBfBI7D7XKH8dKI6aoJRwY5wM5VargrCQ25NFqPGovfDpACiEK+2xJb1hNx+fwBgUWwKAA):

```js
let product = Math.sqrt(x) * y;
// `product` has type `number`
let Component = ({prop}: Props) => { return <>{prop}</> }
// `Component` has type`React.ComponentType<Props>`
let element = <Component {...props} />
// `element` has type `React.Element<React.ComponentType<Props>>`
```
Any subsequent assignments to `product`, `Component`, or `element` will be checked against the types that Flow infers for the initializers, and if conflicting types are assigned, Flow will signal an error [[example]](https://flow.org/try/#0JYWwDg9gTgLgBAKjgQwM5wEoFNkGN4BmUEIcA5FDvmQNwBQdMAnmFnAArFjoC8cA3gB84YLgC44IYAA8sAEziCAvvTpysuADbJKcAG4640iQDsAriABGWKPXVadbA1DhNTF67bUbtu5yK5UCU4IblVNLHhRCDkzfDg+AFlkGAALADpUAEdYAAppAEo4AGpXegB6crgAA2jY-Gq4VLQ4ZlYa8ysbaroI+ABhEkgTLBN4Plz+aLAlYMCingA+AThKGDMoEzgAHkWpriVt8uWlOkqawfAIEbHG5vQ2rGrsPBh0y+HRmAAVFixtkLcRY9PpwLAREBfBI7D7XKH8dKI6aoJRwY5wM5VargrCQ25NFqPGovfDpACiEK+2xJb1hNx+fwBgUWwIYRIA8mkbIDeAJhBAuVAAPrTCRSWQKZR2HyOfSGTmpGx0r4SXIK7nzBLLGnpAByMSw0ocfkMAsVwuREnVUB5DHO+pgbDSKTguVwG0oY00TCKMCgTAAtARNBAAO5wOQQLCoExkeDNPROxVwEbh6wJ4DQOAQAhwMwmZAmEwClLyOVQYDISwRVCYsEF6vyAA0cFQEDgwHgkejsfgqCwSejbBaNmIUFQ6TodTi4zgACJ2RsU1hw9P4sAJ4i5zQ0VUyVAx3RlWNoZMzTYRQcrYKeQtlvxVpENltdvxzxaDkcTjvzvvDzi8VnbZrWPeAEURd9L1CFE0UWH89wPaAgA):

```js
product = "Our new product is..."; // Error
Component = ({other_prop}: OtherProps) => { return <>{other_prop}</> }; // Error
element = <OtherComponent {...other_props} />; // Error
```
If you want these examples to typecheck, and for Flow to realize that different kinds of values can be written to these variables, you must add a type annotation reflecting this more general type to their declarations [[example]](https://flow.org/try/#0JYWwDg9gTgLgBAKjgQwM5wEoFNkGN4BmUEIcA5FDvmQNwBQdMAnmFnAArFjoC8cA3gB84YLgC44IYAA8sAEziCAvvTpysuADbJKcAG4640iQDsAriABGWKPXVadbA1DhNTF67bUbtu5yK5UCU4IblVNLHhRCDkzfHcrG0U4VBgoYBMAczg+AFlkGAALADpUAEdYAAppAEo4AGpXcMi4AGESSBMsExgJKVkFPkr+aLAlYMC6ngA+AThKGDMoEzgAHmmRriVVgHpZpTodnbgAOQg4TIgYuGZWG-PKUSxUbvgi4FQAQkxntLjF3S4GJsDKpHByBgReBYCIgV4SbB4GDFM7qHJraa7WYMRgsNgAeSKNhC3HRQjgECJUAA+qM+jJ5IoVN4HH5DITCjZ2uAIF0ehJKhziZMcrNEfgUcC7D5HPpDJTOTTRkE4EKoCTUAxorF8OiAET4pZwLoAdwCMX+cA+xRtevo3M6r3RwwVNlpWwkao1U1m-HmkSWK3W-FdSq2WLgKjgdBhWDhPXRqzVDt5Tv4NuKofdoVQSjgexocCAA):

```js
let product: number | string = ...
let Component: mixed = ... // No good type to represent this! Consider restructuring
let element: React.Node = ...
```
### Variables declared without initializers {#toc-variables-declared-without-initializers}

Often variables are declared without initializers. In such cases, Flow will try to choose the “first” assignment or assignments to the variable to define its type. “First” here means both top-to-bottom and nearer-scope to deeper-scope—we’ll try to choose an assignment that happens in the same function scope as the variable’s declaration, and only look inside nested functions if we don’t find any assignments locally [[example]](https://flow.org/try/#0DYUwLgBGD2AOAyIBuJgEEDOGCWBzAdiACYDcAUGQGYCu+AxmNtPhABaqwgBOAFAJQQA3mQhQ4iFOix5CRCAF4IAFgBMJCAHoNEAKJcu0LhGwtqsOtAC2J3BBRcczCNEoQAYsGgB3AFwQABjAIyKiYOATE-mwAhhhQAJ6cARhgXDb+ZAC+FEESodIRcooARAASqJ4QXobARMXqWhAAKqzYcV5pYCAQRODc1oRxYOx20VwA5EOJIGS5IVLhsgpQXNQgDdp6BkYmEGYWA7b2jiwu7p6+AXOSYTKRMVNJ-ilp+Lj+QA).

```js flow-check
let topLevelAssigned;
function helper() {
  topLevelAssigned = 42; // Error: `topLevelAssigned` has type `string`
}
topLevelAssigned = "Hello world"; // This write determines the var's type
topLevelAssigned = true; // Error: `topLevelAssigned` has type `string`
```
If there are two or more possible “first assignments,” due to an if or switch statement, they’ll both count—this is one of the few ways that Flow will still infer unions for variable types [[example]](https://flow.org/try/#0CYUwxgNghgTiAEA3W8wHsB2wCWAXbmAXPAEZpoQhQYDcAUHZbvALYCeAcgK4skgwB5GAGVcMbBgDm9OtgBm8ABToseAhgCU8AN514rTjz6CRYiZPgBeeABYATPQC+8EBADOCXfvbde-IaLiUlbwAEQAEq4QaPAA7mgwEMChTgw+Rv6mQRbWdgCMNPAA9EXwchIgdOl+JoHmIaEA4uTAJGwgKcWl5RiV1cYBZsHWclDuIIUl8ACiMDAJ8BLwXAAO6Cz1iPxu6vBoCgBi0bHEAAb9mXVSp-AAFlBu8LhsKwinGBkw8AA+8G5DklOQA):

```js flow-check
let myNumberOrString;
declare var condition: boolean;
if (condition) {
  myNumberOrString = 42; // Determines type
} else {
  myNumberOrString = "Hello world"; // Determines type
}
myNumberOrString = 21; // fine, compatible with type
myNumberOrString = "Goodbye"; // fine, compatible with type
myNumberOrString = false; // Error: `myNumberOrString` has type `number | string`
```
This only applies when the variable is written to in both branches, however. If only one branch contains a write, that write becomes the type of the variable afterwards (though Flow will still check to make sure that the variable is definitely initialized) [[example]](https://flow.org/try/#0CYUwxgNghgTiAEA3W8wHsB2wCWAXbmAXPAEZpoQhQYDcAUHZbvJiAEIzVgAWAggM79sAcwwhg9OtgBm8ABToseAhgCU8AN514LMRy59BIscHgBeeACIAEiAgQ08AO5oYEYAEJL9AL4NW+hg8AkKi4gB0uGgAqgAOsSAwAMJQ-CByqjTwAPTZ8ACiMDCuxAAGAZxBhqEmpfAAtlAAnqQIAK4Y2J34UBDYAF7idBUGIcbi5vAALABMWbkFRa7wXfBtsej1XcJIiUKYLLIAYg5OZSNVY2HAddyp8LhNCfCl-Lgw26VAA):

```js flow-check
let oneBranchAssigned;
declare var condition: boolean;
if (condition) {
  oneBranchAssigned = "Hello world!";
}
oneBranchAssigned.toUpperCase(); // Error: `oneBranchAssigned` may be uninitialized
oneBranchAssigned = 42; // Error: `oneBranchAssigned` has type `string`
```
### Variables initialized to `null` {#toc-variables-initialized-to-null}

Finally, the one exception to the general principle that variable’s types are determined by their first assignment(s) is when a variable is initialized as (or whose first assignment is) the literal value `null`. In such cases, the *next* non-null assignment (using the same rules as above) determines the rest of the variable’s type, and the overall type of the variable becomes a union of `null` and the type of the subsequent assignment. This supports the common pattern where a variable starts off as `null` before getting assigned by a value of some other type [[example]](https://flow.org/try/#0GYVwdgxgLglg9mABMGYAmBJAIgNQIYA2IApgDwAqAfABRozQBciA3gNoDWxAnkwM5QAnVAHMAuk3IBfAJQSWAKESICxKIhhp8BRAF5EYEAQIBuRAHoz6sDFiEYAL2JpEeXogAGBo+8XI4AxGoIBH5ETi4rRDpoaQUlJRhgQPDdHT0Acmx02OZfePVNQl0o+igOblFTC0QMMGBiAKgACzw1dw0td0QWtyguAAdiDy9tAB9Ech98yV8ZhKTqDqK0vRGcvMRmgTgAd31iPYBRAW2BagAiADk4RGIwQQjgfxqsAEJz6WNZ3wFVEAEkEsTPJJEA).

```js flow-check
function findIDValue<T>(dict: {[key: string]: T}): T {
  let idVal = null; // initialized as `null`
  for (const key in dict) {
    if (key === 'ID') {
      idVal = dict[key]; // Infer that `idVal` has type `null | T`
    }
  }
  if (idVal === null) {
    throw new Error("No entry for ID!");
  }
  return idVal;
}
```
