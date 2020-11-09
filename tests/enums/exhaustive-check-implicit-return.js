// @flow

enum E {
  A,
  B,
}

enum F {
  C,
  D,
}

function a(x: E): number { // OK
  switch (x) {
    case E.A:
      return 1;
    case E.B:
      return 2;
  }
}

function b(x: E): number { // OK
  if (Math.random() > 0.5) {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  } else {
    switch (x) {
      case E.A:
        return 3;
      case E.B:
        return 4;
    }
  }
}

function c(x: E, y: F): number { // OK
  switch (x) {
    case E.A:
      switch (y) {
        case F.C:
          return 1;
        case F.D:
          return 2;
      }
    case E.B:
      switch (y) {
        case F.C:
          return 3;
        case F.D:
          return 4;
      }
  }
}

function d(x: E): number { // OK
  switch (x) {
    case E.A:
      return 1;
    case E.B:
      return 2;
  }
  switch (x) {
    case E.A:
      return 1;
    case E.B:
      return 2;
  }
}

function e(x: E): number { // OK
  try {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  } finally {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  }
}

function f(x: E): number { // OK
  try {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  } catch (_) {
    return 3;
  }
}

function g(x: E): number { // OK
  switch (x) {
    case E.A:
    case E.B:
      return 1;
  }
}

function h(x: E, y: F): number { // OK
  switch (x) {
    case E.A:
    case E.B:
      switch (y) {
        case F.C:
          return 3;
        case F.D:
          return 4;
      }
  }
}

function* i(x: E): Generator<void, number, void> {
  switch (x) {
    case E.A:
      return 1;
    case E.B:
      return 2;
  }
}

async function j(x: E): Promise<number> {
  switch (x) {
    case E.A:
      return 1;
    case E.B:
      return 2;
  }
}

function k(x: ?E): string {
  if (x == null) {
    return 'c';
  }
  switch (x) {
    case E.A:
      return 'a';
    case E.B:
      return 'b';
  }
}

// Errors
function q(x: E): number { // Error: switch isn't exhaustive
  switch (x) { // Error
    case E.A:
      return 1;
  }
}

function r(x: E): number { // Error
  if (Math.random() > 0.5) {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  }
}

function s(x: E): number { // Error: second switch isn't exhaustive
  if (Math.random() > 0.5) {
    switch (x) {
      case E.A:
        return 1;
      case E.B:
        return 2;
    }
  } else {
    switch (x) { // Error
      case E.A:
        return 3;
    }
  }
}

function t(x: any): number { // Error
  switch (x) {
    case E.A:
      return 1;
  }
}

function* u(x: E): Generator<void, number, void> { // Error: switch isn't exhaustive
  switch (x) { // Error
    case E.A:
      return 1;
  }
}

async function v(x: E): Promise<number> { // Error: switch isn't exhaustive
  switch (x) { // Error
    case E.A:
      return 1;
  }
}
