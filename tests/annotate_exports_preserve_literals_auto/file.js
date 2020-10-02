// @flow

export function tag_like_values() {
  return {
    tag_like: 'tag_like', // should print '$TEMPORARY$string<"tag_like">'
    non_tag_like: 'non_tag_like!', // should print 'string'
  };
}

// should print '$TEMPORARY$array<...>'
export function array_literal(x: string, y: number) {
  return [x, y];
}

// should print 'boolean'
export function bool_literal() {
  return true;
}

// should print 'number
export function number_literal() {
  return 1;
}
