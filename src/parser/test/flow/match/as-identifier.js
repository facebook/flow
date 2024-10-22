type match = number;
{
  const match = 1;
}
function match(match: match) {}

const a = match(1);

const b = match(1).f();

const c = match(1)
{
  // block statement
}

match(1);

match(1).f();

match(1)
{
  // block statement
}
