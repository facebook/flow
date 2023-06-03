function return_annot_refers_to_param(x: number): typeof x {
  return x; // okay
}

function return_annot_refers_to_rest(...x: Array<number>): typeof x {
  return x; // okay
}
