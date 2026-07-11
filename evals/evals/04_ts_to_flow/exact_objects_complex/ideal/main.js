/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Circle = {type: 'circle', radius: number};
type Square = {type: 'square', side: number};
type Rectangle = {type: 'rectangle', width: number, height: number};

type Shape = Circle | Square | Rectangle;

function getShapeArea(shape: Shape): number {
  return match (shape) {
    {type: 'circle', const radius} => Math.PI * radius * radius,
    {type: 'square', const side} => side * side,
    {type: 'rectangle', const width, const height} => width * height,
  };
}

function getShapePerimeter(shape: Shape): number {
  return match (shape) {
    {type: 'circle', const radius} => 2 * Math.PI * radius,
    {type: 'square', const side} => 4 * side,
    {type: 'rectangle', const width, const height} => 2 * (width + height),
  };
}

function isCircle(shape: Shape): shape is Circle {
  return shape.type === 'circle';
}

function isSquare(shape: Shape): shape is Square {
  return shape.type === 'square';
}

const circle: Circle = {type: 'circle', radius: 5};
const square: Square = {type: 'square', side: 4};
const rectangle: Rectangle = {type: 'rectangle', width: 6, height: 3};

const shapes: Array<Shape> = [circle, square, rectangle];
const circles = shapes.filter(isCircle);
const squares = shapes.filter(isSquare);

console.log(`Found ${circles.length} circles`);
console.log(`Found ${squares.length} squares`);
console.log(`Total area: ${shapes.map(getShapeArea).reduce((a, b) => a + b, 0)}`);
console.log(
  `Total perimeter: ${shapes.map(getShapePerimeter).reduce((a, b) => a + b, 0)}`,
);
