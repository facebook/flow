declare abstract class Base { // ERROR: abstract classes unconditionally error for now
  abstract getName(): string;
  greet(): string;
}
