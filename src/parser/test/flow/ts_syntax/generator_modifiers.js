class Derived extends Base {
  *plain() {}
  override *m() { yield 1; }
  public *publicGenerator() {}
  private *privateGenerator() {}
  protected *protectedGenerator() {}
  public override *publicOverride() {}
  private override *privateOverride() {}
  protected override *protectedOverride() {}
  static override *staticOverride() {}
  public static override *publicStaticOverride() {}
  override *[key]() {}
  override *#privateName() {}
  override *"literal"() {}
  override /* comment */ *withComment() {}
  override
  *withNewline() {}
  override async *asyncGenerator() {}
  public override async *publicAsyncGenerator() {}
  override() {}
  public() {}
}
