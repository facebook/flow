//@flow

declare var f: {|
  +x: number
|};

f.x++;
f.x--;
f.x += 0;
f.x -= 0;
f.x /= 0;
f.x %= 0;
f.x <<= 0;
f.x >>= 0;
f.x >>>= 0;
f.x |= 0;
f.x ^= 0;
f.x &= 0;
