{
  record R {
    static: number,
    async: number,
    static async: number = 1,
  }
}
{
  record R {
    static() {}
    async() {}
    static async() {}
  }
}
{
  record R {
    static<T>() {}
    async<T>() {}
    static async<T>() {}
  }
}
{
  record R {
    async static() {}
  }
}
{
  record R {
    async static<T>() {}
  }
}
