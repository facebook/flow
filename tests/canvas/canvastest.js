var el = document.createElement('canvas');
var ctx: ?CanvasRenderingContext2D = el.getContext('2d');
if (ctx == null) {
  // error handling
} else {
  // ctx here is specifically a CanvasRenderingContext2D (not a WebGL context)
  ctx.fillRect(0, 0, 200, 100);
  ctx.moveTo('0', '1');  // error: should be numbers
}
