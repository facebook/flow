interface Circle {
  type: "circle";
  radius: number;
}

interface Square {
  type: "square";
  side: number;
}

interface Rectangle {
  type: "rectangle";
  width: number;
  height: number;
}

type Shape = Circle | Square | Rectangle;

function getShapeArea(shape: Shape): number {
  switch (shape.type) {
    case "circle":
      return Math.PI * shape.radius * shape.radius;
    case "square":
      return shape.side * shape.side;
    case "rectangle":
      return shape.width * shape.height;
    default:
      throw new Error(`Unknown shape type`);
  }
}

function getShapePerimeter(shape: Shape): number {
  switch (shape.type) {
    case "circle":
      return 2 * Math.PI * shape.radius;
    case "square":
      return 4 * shape.side;
    case "rectangle":
      return 2 * (shape.width + shape.height);
    default:
      throw new Error(`Unknown shape type`);
  }
}

function isCircle(shape: Shape): shape is Circle {
  return shape.type === "circle";
}

function isSquare(shape: Shape): shape is Square {
  return shape.type === "square";
}

const circle: Circle = { type: "circle", radius: 5 };
const square: Square = { type: "square", side: 4 };
const rectangle: Rectangle = { type: "rectangle", width: 6, height: 3 };

const shapes: Shape[] = [circle, square, rectangle];
const circles = shapes.filter(isCircle);
const squares = shapes.filter(isSquare);

console.log(`Found ${circles.length} circles`);
console.log(`Found ${squares.length} squares`);
console.log(`Total area: ${shapes.map(getShapeArea).reduce((a, b) => a + b, 0)}`);
console.log(`Total perimeter: ${shapes.map(getShapePerimeter).reduce((a, b) => a + b, 0)}`);
