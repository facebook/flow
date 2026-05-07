import type {Opaque, State} from './opaque_type.js'

type Obj = {[id: Opaque]: void};

declare const foo1 : null | Opaque;
declare const bar : null | Obj
if (foo1 != null && bar) {
  bar[foo1];
}


function foo2(state: State) {
  const id = state.o;
  if (id != null && state.d) {
    state.d.index[id];
  }
}
