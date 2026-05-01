function length_refinement(a: [string] | [number, string]): void {
  if (a.length === 1) {
    a[0] as string;
  } else {

    a[0] as number;
    a[1] as string;
  }
}

function partial_length_refinement(
  a: [string] | [number, number] | [number, number, number]
): void {
  if (a.length === 1) {
    a[0] as string;
  } else {
    a[0] as number;
    a[1] as number;
  }
}

function partial_length_refinement_not_specific_enough(
  a: [string] | [number, number] | [number, number, number]
): void {
  if (a.length === 1) {
    a[0] as string;
  } else {
    a[0] as number;
    a[1] as number;
    a[2] as number; // Error: one of the variants has only 2 elements.
  }
}

function refine_on_negative_length(a: [number] | [string, number]): void {
  if (a.length === -1) {
    a as empty;
  }
}

function refine_on_length_that_is_too_large(a: [number] | [string, number]): void {
  if (a.length === 7) {
    a as empty;
  }
}

function check_no_rounding(a: [number] | [string, number]): void {
  if (a.length === 0.9) {
    a as empty;
  }
  if (a.length === 1.1) {
    a as empty;
  }
  if (a.length === 1.9) {
    a as empty;
  }
}

function length_refinement_with_switch(a: [string] | [number, string]): void {
  switch (a.length) {
  case 1:
    a[0] as string;
    break;
  default:
    a[0] as number;
    a[1] as string;
  }
}
