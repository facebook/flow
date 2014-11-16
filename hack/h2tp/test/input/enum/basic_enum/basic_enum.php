<?hh
enum DayOfWeek: int {
  Sunday = 0;
  Monday = 1;
  Tuesday = 2;
  Wednesday = 3;
  Thursday = 4;
  Friday = 5;
  Saturday = 6;
}

function foo(): DayOfWeek {
  return DayOfWeek::Wednesday;
}
echo (foo()."\n");