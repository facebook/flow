function longestLine(lines: Iterable<string>): number {
  let best = 0;
  for (const line of lines) {
    if (line.length > best) {
      best = line.length;
    }
  }
  return best;
}

const changelog = [
  "Fix crash on empty input",
  "Update dependency versions",
  "Ship native module",
];
console.log("changelog max =", longestLine(changelog));

const commitTitle = "Ship native module";
console.log("commit max =", longestLine(commitTitle));
