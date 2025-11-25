// Semicolon suffix: single
{
  record R {
    a: number;
  }
}
{
  record R {
    static a: number = 1;
  }
}

// Semicolon suffix: multiple
{
  record R {
    a: number;
    static b: string = 's';
  }
}
