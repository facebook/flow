const a = ["a", "b"].reduce(acc => acc * 2, 1.0); // works
const b = ["a", "b"].reduce(acc => acc * 2, 1.0 as any); // works
