class InitializedParent {
  x: number;
  constructor() { this.x = 1; }
}

class RelistedWithoutConstructor extends InitializedParent {
  x: number; // error
}

class DeclaredWithoutConstructor extends InitializedParent {
  declare x: number;
}

class CommentDeclaredWithoutConstructor extends InitializedParent {
  /*:: declare x: number; */
}
