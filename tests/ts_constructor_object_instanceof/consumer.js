import {
  BoxLikeConstructor,
  DerivedHTMLElementLike,
  HTMLElementLike,
  MultiParentChild,
  OverloadedHTMLElementLike,
  UnionReturningHTMLElementLike,
  element,
  multiParentBase,
  multiParentUnion,
  overloadedOrOther,
  overloadedValue,
  unionValue,
  unknownValue,
} from './typescript';

if (element instanceof HTMLElementLike) {
  element.tagName === 'div';
  element.htmlOnly as number;
  element.htmlOnly as string; // ERROR: the refinement is not `any` or `empty`
}

if (unknownValue instanceof HTMLElementLike) {
  unknownValue.tagName as string;
  unknownValue.htmlOnly as number;
  unknownValue.tagName as number; // ERROR: `unknown` refines to the instance type
}

if (element instanceof DerivedHTMLElementLike) {
  element.htmlOnly as number;
  element.htmlOnly as string; // ERROR: inherited construct signatures refine
}

if (element instanceof OverloadedHTMLElementLike) {
  element.htmlOnly as number;
  element.htmlOnly as string; // ERROR: every overload returns an HTML-like instance
}

if (!(element instanceof HTMLElementLike)) {
  element.tagName as string;
  element.htmlOnly; // ERROR: the negative branch retains the original supertype
}

if (unionValue instanceof HTMLElementLike) {
  unionValue.htmlOnly as number;
  unionValue.other; // ERROR: unrelated union members are pruned
}

// Failing the check means failing it against every overload, so a scrutinee
// that every overload can build is pruned away: the branch is unreachable and
// even a nonexistent property reads without complaint.
if (!(overloadedValue instanceof OverloadedHTMLElementLike)) {
  overloadedValue.noSuchProp;
}

// A member no overload can build still survives that same check.
if (!(overloadedOrOther instanceof OverloadedHTMLElementLike)) {
  overloadedOrOther.other as string;
  overloadedOrOther.tagName; // ERROR: only the unrelated member survives
}

// One signature returning a union prunes as much as two signatures do.
if (!(unionValue instanceof UnionReturningHTMLElementLike)) {
  unionValue.noSuchProp;
}

if (!(element instanceof UnionReturningHTMLElementLike)) {
  element.tagName as string;
  element.htmlOnly; // ERROR: a supertype of both return types is not pruned
}

// A polymorphic signature instantiates its parameters rather than dropping out,
// so this refines to `BoxLike<any>` instead of leaving a dead branch.
if (unknownValue instanceof BoxLikeConstructor) {
  unknownValue.value;
  unknownValue.tagName; // ERROR: the refinement is `BoxLike<any>`, not `empty`
}

if (multiParentBase instanceof MultiParentChild) {
  multiParentBase.child as number;
  multiParentBase.child as string; // ERROR: multiple interface parents still permit a downcast
  multiParentBase as empty; // ERROR: the positive branch is not `empty`
}

if (!(multiParentBase instanceof MultiParentChild)) {
  multiParentBase.base as string;
  multiParentBase.base as number; // ERROR: the negative branch retains the base interface
}

if (multiParentUnion instanceof MultiParentChild) {
  multiParentUnion.child as number;
  multiParentUnion.other; // ERROR: the unrelated union member is pruned
}
