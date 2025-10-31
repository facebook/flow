record R {
  foo: string,

  f_tparams<T>() {}

  static f_static(): void {}

  async f_async() {}

  *f_generator() {}

  async *f_async_generator() {}

  static *f_static_generator() {}

  static async f_static_async() {}

  static async *f_all() {}
}
