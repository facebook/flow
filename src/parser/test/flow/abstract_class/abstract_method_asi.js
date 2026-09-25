abstract class BeforeClosingBrace {
  abstract method(): void
}

abstract class BeforeNextMember {
  abstract first(): void
  abstract second(): void;
}

abstract class BeforeLineComment {
  abstract first(): void // comment
  abstract second(): void;
}
