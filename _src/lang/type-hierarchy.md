---
title: Type Hierarchy
slug: /lang/type-hierarchy
---

Types in Flow form a hierarchy based on [subtyping](../subtypes):

```mermaid
graph BT

mixed -.- any

symbol --> mixed
null --> mixed
maybe["Maybe:
  ?string"]
maybe --> mixed
null --> maybe
void --> maybe
void --> mixed
string --> maybe
string --> mixed

union["Union:
  number | bigint"]
number --> union
number --> mixed
union --> mixed
bigint --> mixed
bigint --> union

boolean --> mixed
true --> boolean
false --> boolean

empty-interface["interface {}"] --> mixed
some-interface["interface {prop: string}"] --> empty-interface
someclass["class A {prop: string}"] --> some-interface
inexact-empty-obj["Inexact empty object:
  {...}"]
inexact-empty-obj --> empty-interface
inexact-some-obj["Inexact object:
  {prop: string, ...}"] --> inexact-empty-obj
inexact-some-obj --> some-interface
exact-some-obj["Exact object:
  {prop: string}"] --> inexact-some-obj
exact-empty-obj["Exact empty object:
  {}"]
exact-empty-obj --> inexact-empty-obj
roarray-mixed["$ReadOnlyArray&lt;mixed>"] --> empty-interface
inexact-empty-tuple["Inexact empty tuple:
  [...]"]
some-tuple["Tuple:
  [string, number]"]
inexact-empty-tuple --> roarray-mixed
some-tuple --> inexact-empty-tuple
some-array["Array&lt;string>"] --> roarray-mixed

any-func["Function:
  (...$ReadOnlyArray&lt;empty>) => mixed"]
any-func --> empty-interface
some-func["(number) => boolean"] --> any-func
some-func2["(string) => string"] --> any-func

inter["Intersection:
  (number => boolean) & (string => string)"]
inter --> some-func
inter --> some-func2

empty --> inter
empty --> null
empty --> void
empty --> true
empty --> false
empty --> exact-some-obj
empty --> exact-empty-obj
empty --> some-tuple
empty --> some-array
empty --> string
empty --> number
empty --> bigint
empty --> someclass
empty --> symbol
any-bottom["any"] -.- empty

click mixed "../../types/mixed"
click any "../../types/any"
click any-bottom "../../types/any"
click empty "../../types/empty"
click boolean "../../types/primitives/#toc-booleans"
click number "../../types/primitives/#toc-numbers"
click string "../../types/primitives/#toc-strings"
click symbol "../../types/primitives/#toc-symbols"
click bigint "../../types/primitives/#toc-bigints"
click null "../../types/primitives/#toc-null-and-void"
click void "../../types/primitives/#toc-null-and-void"
click true "../../types/literals"
click false "../../types/literals"
click union "../../types/unions"
click inter "../../types/intersections"
click maybe "../../types/maybe"
click some-array "../../types/arrays"
click roarray-mixed "../../types/arrays/#toc-readonlyarray"
click inexact-empty-tuple "../../types/tuples/#inexact-tuples"
click some-tuple "../../types/tuples"
click someclass "../../types/classes"
click empty-interface "../../types/interfaces"
click some-interface "../../types/interfaces"
click exact-some-obj "../../types/objects"
click exact-empty-obj "../../types/objects"
click inexact-some-obj "../../types/objects/#exact-and-inexact-object-types"
click inexact-empty-obj "../../types/objects/#exact-and-inexact-object-types"
click any-func "../../types/functions"
click some-func "../../types/functions"
click some-func2 "../../types/functions"

classDef default fill:#eee, stroke:#000, stroke-width:1px
```

Click on a node to go to the documentation for that type.

Types appearing higher in this graph are more general, while those appearing lower are more specific.
An arrow pointing from type `A` to type `B` means that `A` is a subtype of `B`.
For example, the type `string` is a subtype of `?string`.

How can `any` be at both the top and the bottom? Because [it is unsafe](../../types/any/). This is denoted with a dotted line.
