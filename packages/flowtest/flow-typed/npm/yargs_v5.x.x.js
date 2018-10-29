// flow-typed signature: f886221205e379304f1aba426495bd77
// flow-typed version: da30fe6876/yargs_v5.x.x/flow_>=v0.25.x

declare module "yargs" {
  declare type Argv = { _: Array<string>, [key: string]: mixed };

  declare type Options = $Shape<{
    alias: string,
    array: boolean,
    choices: Array<any>,
    config: boolean,
    configParser: (configPath: string) => { [key: string]: mixed },
    count: boolean,
    default: boolean,
    defaultDescription: string,
    demand: boolean,
    desc: string,
    describe: string,
    description: string,
    global: boolean,
    group: string,
    nargs: number,
    normalize: boolean,
    number: boolean,
    require: boolean,
    required: boolean,
    requiresArg: boolean,
    string: boolean,
    type: "array" | "boolean" | "count" | "number" | "string"
  }>;

  declare type DescParseFn = (configPath: string) => Object;

  declare type ModuleObject = {
    command: string,
    describe: string,
    builder: { [key: string]: Options } | ((yargsInstance: Yargs) => mixed),
    handler: (argv: Argv) => void
  };

  declare class Yargs {
    alias(toBeAliased: string, alias: string): this;
    argv: Argv;
    array(key: string | Array<string>): this;
    boolean(paramter: string | Array<string>): this;
    check(fn: (argv: Argv, options: Array<string>) => ?boolean): this;
    choices(key: string, allowed: Array<string>): this;

    command(
      cmd: string,
      desc: string | false,
      builder?: { [key: string]: Options } | ((yargsInstance: Yargs) => mixed),
      handler?: Function
    ): this;

    command(cmd: string, desc: string | false, module: ModuleObject): this;

    command(module: ModuleObject): this;

    completion(
      cmd: string,
      description?: string,
      fn?: (
        current: string,
        argv: Object,
        done: (competion: Array<string>) => void
      ) => ?(Array<string> | Promise<Array<string>>)
    ): this;

    config(
      key: string,
      description?: string | DescParseFn,
      parseFn?: DescParseFn
    ): this;

    count(name: string): this;

    default(defaultObject: { [paramter: string]: any }): this;
    default(parameter: string, value: any): this;

    // Alias of require()!
    demand(key: string, msg: string | boolean): this;
    demand(count: number, max?: number, msg?: string | boolean): this;

    describe(key: string, desc: string): this;
    describe(describeObject: { [key: string]: string }): this;

    detectLocale(shouldI: boolean): this;

    env(prefix?: string): this;

    epilog(epi: string): this;
    epilogue(epi: string): this;

    example(cmd: string, desc: string): this;

    exitProcess(enable: boolean): this;

    fail(fn: (failureMessage: string) => mixed): this;

    global(globals: string | Array<string>): this;

    group(key: string | Array<string>, groupName: string): this;

    help(option?: string, desc?: string): this;

    implies(keyA: string, keyB: string): this;
    implies(keys: { [key: string]: string }): this;

    locale(
      locale: | "de"
      | "en"
      | "es"
      | "fr"
      | "id"
      | "it"
      | "ja"
      | "ko"
      | "nb"
      | "pirate"
      | "pl"
      | "pt"
      | "pt_BR"
      | "tr"
      | "zh"
    ): this;
    locale(): string;

    nargs(key: string, count: number): this;

    normalize(key: string): this;

    number(key: string | Array<string>): this;

    option(key: string, options: Options): this;
    option(optionMap: { [key: string]: Options }): this;

    options(key: string, options: Options): this;
    options(optionMap: { [key: string]: Options }): this;

    parse(args: string | Array<string>): Argv;

    pkgConf(key: string, cwd?: string): this;

    // Alias of demand()!
    require(key: string, msg: string | boolean): this;
    require(count: number, max?: number, msg?: string | boolean): this;

    requiresArg(key: string | Array<string>): this;

    reset(): this;

    showCompletionScript(): this;

    showHelp(consoleLevel: "error" | "log" | "warn"): this;

    showHelpOnFail(enable: boolean, message?: string): this;

    strict(): this;

    string(key: string | Array<string>): this;

    updateLocale(obj: { [key: string]: string }): this;
    updateStrings(obj: { [key: string]: string }): this;

    usage(message: string, opts?: { [key: string]: Options }): this;

    version(
      option?: string | (() => string),
      description?: string | Function,
      version?: string | Function
    ): this;

    wrap(columns: number | null): this;
  }

  declare module.exports: Yargs;
}
