/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noformat
 * @oncall flow
 */

declare class SVGElement extends HTMLElement {
  viewportElement: ?SVGElement;
  ownerSVGElement: ?SVGSVGElement;
  xmlbase: string;
  id: string;
}

declare class SVGLocatable extends SVGElement {
  getBBox(): SVGRect;
  getTransformToElement(element: SVGElement): SVGMatrix;
}

declare class SVGRect {
  x: number;
  y: number;
  width: number;
  height: number;
}

declare class SVGSVGElement extends SVGLocatable {
  createSVGPoint(): SVGPoint;
  getScreenCTM(): SVGMatrix;
}

declare class SVGPoint extends SVGElement {
  x: number;
  y: number;
  matrixTransform(matrix: SVGMatrix): SVGPoint;
}

// $FlowFixMe[libdef-override]
declare class SVGMatrix {
  multiply(secondMatrix: SVGMatrix): SVGMatrix;
  inverse(): SVGMatrix;
  translate(x: number, y: number): SVGMatrix;
  scale(scaleFactor: number): SVGMatrix;
  scaleNonUniform(scaleFactorX: number, scaleFactorY: number): SVGMatrix;
  rotate(angle: number): SVGMatrix;
  rotateFromVector(x: number, y: number): SVGMatrix;
  flipX(): SVGMatrix;
  flipY(): SVGMatrix;
  skewX(angle: number): SVGMatrix;
  skewY(angle: number): SVGMatrix;
}
