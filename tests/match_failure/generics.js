// @flow

function f1<T: 's' | 'p'>(mm: { type: T }) {
  if (mm.type === 's'); // Okay
  if (mm.type === 'q'); // Error 'q' is not included in 's' | 'p'
}

type Message = { type: 's', v: number } | { type: 'p', x: string };

function f2<M: Message>(m_orig: M) {
  const mm: M = m_orig;

  if (mm.type === 's'); // Okay
  if (mm.type === 'q'); // Error 'q' is not included in 's' | 'p'
}
