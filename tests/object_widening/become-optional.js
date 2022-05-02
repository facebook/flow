//@flow

declare function maybeNumber(): ?number;

let foo = {
  foo1: maybeNumber(),
  foo2: maybeNumber(),
  foo3: maybeNumber(),
  foo4: maybeNumber(),
  foo5: maybeNumber(),
  foo6: maybeNumber(),
  foo7: maybeNumber(),
  foo8: maybeNumber(),
  foo9: maybeNumber(),
  foo10: maybeNumber(),
  foo11: maybeNumber(),
  foo12: maybeNumber(),
  foo13: maybeNumber(),
};

const f1 = () => {foo = {...foo, foo1: 3}};
const f2 = () => {foo = {...foo, foo2: 3}};
const f3 = () => {foo = {...foo, foo3: 3}};
const f4 = () => {foo = {...foo, foo4: 3}};
const f5 = () => {foo = {...foo, foo5: 3}};
const f6 = () => {foo = {...foo, foo6: 3}};
const f7 = () => {foo = {...foo, foo7: 3}};
const f8 = () => {foo = {...foo, foo8: 3}};
const f9 = () => {foo = {...foo, foo9: 3}};
const f10 = () => {foo = {...foo, foo10: 3}};
const f11 = () => {foo = {...foo, foo11: 3}};
const f12 = () => {foo = {...foo, foo12: 3}};
const f13 = () => {foo = {...foo, foo13: 3}};
