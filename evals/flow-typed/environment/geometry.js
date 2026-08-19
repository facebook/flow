/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noformat
 * @oncall flow
 */

// https://www.w3.org/TR/geometry-1/

type DOMMatrix2DInit =
  | {|
      a: number,
      b: number,
      c: number,
      d: number,
      e: number,
      f: number,
    |}
  | {|
      m11: number,
      m12: number,
      m21: number,
      m22: number,
      m41: number,
      m42: number,
    |};

type DOMMatrixInit =
  | {|
      ...DOMMatrix2DInit,
      is2D: true,
    |}
  | {|
      ...DOMMatrix2DInit,
      is2D: false,
      m13: number,
      m14: number,
      m23: number,
      m24: number,
      m31: number,
      m32: number,
      m33: number,
      m34: number,
      m43: number,
      m44: number,
    |};

type DOMPointInit = {|
  w: number,
  x: number,
  y: number,
  z: number,
|};

type DOMQuadInit = {|
  p1: DOMPointInit,
  p2: DOMPointInit,
  p3: DOMPointInit,
  p4: DOMPointInit,
|};

type DOMRectInit = {|
  height: number,
  width: number,
  x: number,
  y: number,
|};

declare class DOMMatrix extends DOMMatrixReadOnly {
  a: number;
  b: number;
  c: number;
  d: number;
  e: number;
  f: number;
  m11: number;
  m12: number;
  m13: number;
  m14: number;
  m21: number;
  m22: number;
  m23: number;
  m24: number;
  m31: number;
  m32: number;
  m33: number;
  m34: number;
  m41: number;
  m42: number;
  m43: number;
  m44: number;

  static fromFloat32Array(array32: Float32Array): DOMMatrix;
  static fromFloat64Array(array64: Float64Array): DOMMatrix;
  static fromMatrix(other?: DOMMatrixInit): DOMMatrix;

  constructor(init?: string | Array<number>): void;
  invertSelf(): DOMMatrix;
  multiplySelf(other?: DOMMatrixInit): DOMMatrix;
  preMultiplySelf(other?: DOMMatrixInit): DOMMatrix;
  rotateAxisAngleSelf(
    x?: number,
    y?: number,
    z?: number,
    angle?: number
  ): DOMMatrix;
  rotateFromVectorSelf(x?: number, y?: number): DOMMatrix;
  rotateSelf(rotX?: number, rotY?: number, rotZ?: number): DOMMatrix;
  scale3dSelf(
    scale?: number,
    originX?: number,
    originY?: number,
    originZ?: number
  ): DOMMatrix;
  scaleSelf(
    scaleX?: number,
    scaleY?: number,
    scaleZ?: number,
    originX?: number,
    originY?: number,
    originZ?: number
  ): DOMMatrix;
  setMatrixValue(transformList: string): DOMMatrix;
  skewXSelf(sx?: number): DOMMatrix;
  skewYSelf(sy?: number): DOMMatrix;
  translateSelf(tx?: number, ty?: number, tz?: number): DOMMatrix;
}

declare class DOMMatrixReadOnly {
  readonly a: number;
  readonly b: number;
  readonly c: number;
  readonly d: number;
  readonly e: number;
  readonly f: number;
  readonly is2D: boolean;
  readonly isIdentity: boolean;
  readonly m11: number;
  readonly m12: number;
  readonly m13: number;
  readonly m14: number;
  readonly m21: number;
  readonly m22: number;
  readonly m23: number;
  readonly m24: number;
  readonly m31: number;
  readonly m32: number;
  readonly m33: number;
  readonly m34: number;
  readonly m41: number;
  readonly m42: number;
  readonly m43: number;
  readonly m44: number;

  static fromFloat32Array(array32: Float32Array): DOMMatrixReadOnly;
  static fromFloat64Array(array64: Float64Array): DOMMatrixReadOnly;
  static fromMatrix(other?: DOMMatrixInit): DOMMatrixReadOnly;

  constructor(init?: string | Array<number>): void;
  flipX(): DOMMatrix;
  flipY(): DOMMatrix;
  inverse(): DOMMatrix;
  multiply(other?: DOMMatrixInit): DOMMatrix;
  rotate(rotX?: number, rotY?: number, rotZ?: number): DOMMatrix;
  rotateAxisAngle(
    x?: number,
    y?: number,
    z?: number,
    angle?: number
  ): DOMMatrix;
  rotateFromVector(x?: number, y?: number): DOMMatrix;
  scale(
    scaleX?: number,
    scaleY?: number,
    scaleZ?: number,
    originX?: number,
    originY?: number,
    originZ?: number
  ): DOMMatrix;
  scale3d(
    scale?: number,
    originX?: number,
    originY?: number,
    originZ?: number
  ): DOMMatrix;
  scaleNonUniform(scaleX?: number, scaleY?: number): DOMMatrix;
  skewX(sx?: number): DOMMatrix;
  skewY(sy?: number): DOMMatrix;
  toFloat32Array(): Float32Array;
  toFloat64Array(): Float64Array;
  toJSON(): Object;
  transformPoint(point?: DOMPointInit): DOMPoint;
  translate(tx?: number, ty?: number, tz?: number): DOMMatrix;
  toString(): string;
}

declare class DOMPoint extends DOMPointReadOnly {
  w: number;
  x: number;
  y: number;
  z: number;

  static fromPoint(other?: DOMPointInit): DOMPoint;

  constructor(x?: number, y?: number, z?: number, w?: number): void;
}

declare class DOMPointReadOnly {
  readonly w: number;
  readonly x: number;
  readonly y: number;
  readonly z: number;

  static fromPoint(other?: DOMPointInit): DOMPointReadOnly;

  constructor(x?: number, y?: number, z?: number, w?: number): void;
  matrixTransform(matrix?: DOMMatrixInit): DOMPoint;
  toJSON(): Object;
}

declare class DOMQuad {
  readonly p1: DOMPoint;
  readonly p2: DOMPoint;
  readonly p3: DOMPoint;
  readonly p4: DOMPoint;

  static fromQuad(other?: DOMQuadInit): DOMQuad;
  static fromRect(other?: DOMRectInit): DOMQuad;

  constructor(
    p1?: DOMPointInit,
    p2?: DOMPointInit,
    p3?: DOMPointInit,
    p4?: DOMPointInit
  ): void;
  getBounds(): DOMRect;
  toJSON(): Object;
}

declare class DOMRect extends DOMRectReadOnly {
  height: number;
  width: number;
  x: number;
  y: number;

  constructor(x?: number, y?: number, width?: number, height?: number): void;

  static fromRect(other?: DOMRectInit): DOMRect;
}

declare class DOMRectList {
  readonly length: number;

  @@iterator(): Iterator<DOMRect>;

  item(index: number): DOMRect;
  [index: number]: DOMRect;
}

declare class DOMRectReadOnly {
  readonly bottom: number;
  readonly height: number;
  readonly left: number;
  readonly right: number;
  readonly top: number;
  readonly width: number;
  readonly x: number;
  readonly y: number;

  constructor(x?: number, y?: number, width?: number, height?: number): void;

  static fromRect(other?: DOMRectInit): DOMRectReadOnly;
  toJSON(): Object;
}
