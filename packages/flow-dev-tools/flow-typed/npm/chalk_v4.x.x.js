// flow-typed signature: 79cfa6bcaa67fdb60f10d320da0470fc
// flow-typed version: 6cecea2e51/chalk_v4.x.x/flow_>=v0.104.x

// Adapted from: https://github.com/chalk/chalk/blob/v4.1.2/index.d.ts

declare module "chalk" {
  declare type ForegroundColor =
    | 'black'
    | 'red'
    | 'green'
    | 'yellow'
    | 'blue'
    | 'magenta'
    | 'cyan'
    | 'white'
    | 'gray'
    | 'grey'
    | 'blackBright'
    | 'redBright'
    | 'greenBright'
    | 'yellowBright'
    | 'blueBright'
    | 'magentaBright'
    | 'cyanBright'
    | 'whiteBright';

  declare type BackgroundColor =
    | 'bgBlack'
    | 'bgRed'
    | 'bgGreen'
    | 'bgYellow'
    | 'bgBlue'
    | 'bgMagenta'
    | 'bgCyan'
    | 'bgWhite'
    | 'bgGray'
    | 'bgGrey'
    | 'bgBlackBright'
    | 'bgRedBright'
    | 'bgGreenBright'
    | 'bgYellowBright'
    | 'bgBlueBright'
    | 'bgMagentaBright'
    | 'bgCyanBright'
    | 'bgWhiteBright';

  declare type Color = ForegroundColor | BackgroundColor;

  declare type Modifiers =
    | 'reset'
    | 'bold'
    | 'dim'
    | 'italic'
    | 'underline'
    | 'inverse'
    | 'hidden'
    | 'strikethrough'
    | 'visible';

  declare interface TemplateStringsArray extends ReadonlyArray<string> {
    readonly raw: ReadonlyArray<string>;
  }

  declare type Level = 0 | 1 | 2 | 3;

  declare type ChalkOptions = {
    level?: Level
  };

  declare type ColorSupport = {
    level: Level,
    hasBasic: boolean,
    has256: boolean,
    has16m: boolean
  };

  declare interface Instance {
    new (options?: ChalkOptions): Chalk;
  }

  declare interface Chalk {
    (...text: unknown[]): string,
    (text: TemplateStringsArray, ...placeholders: unknown[]): string,
    Instance: Instance,
    level: Level,
    rgb(r: number, g: number, b: number): Chalk,
    hsl(h: number, s: number, l: number): Chalk,
    hsv(h: number, s: number, v: number): Chalk,
    hwb(h: number, w: number, b: number): Chalk,
    ansi(code: number): Chalk,
    ansi256(index: number): Chalk,
    bgHex(color: string): Chalk,
    bgKeyword(color: string): Chalk,
    bgRgb(r: number, g: number, b: number): Chalk,
    bgHsl(h: number, s: number, l: number): Chalk,
    bgHsv(h: number, s: number, v: number): Chalk,
    bgHwb(h: number, w: number, b: number): Chalk,
    bgAnsi(code: number): Chalk,
    bgAnsi256(index: number): Chalk,
    hex(color: string): Chalk,
    keyword(color: string): Chalk,

    readonly reset: Chalk,
    readonly bold: Chalk,
    readonly dim: Chalk,
    readonly italic: Chalk,
    readonly underline: Chalk,
    readonly inverse: Chalk,
    readonly hidden: Chalk,
    readonly strikethrough: Chalk,

    readonly visible: Chalk,

    readonly black: Chalk,
    readonly red: Chalk,
    readonly green: Chalk,
    readonly yellow: Chalk,
    readonly blue: Chalk,
    readonly magenta: Chalk,
    readonly cyan: Chalk,
    readonly white: Chalk,
    readonly gray: Chalk,
    readonly grey: Chalk,
    readonly blackBright: Chalk,
    readonly redBright: Chalk,
    readonly greenBright: Chalk,
    readonly yellowBright: Chalk,
    readonly blueBright: Chalk,
    readonly magentaBright: Chalk,
    readonly cyanBright: Chalk,
    readonly whiteBright: Chalk,

    readonly bgBlack: Chalk,
    readonly bgRed: Chalk,
    readonly bgGreen: Chalk,
    readonly bgYellow: Chalk,
    readonly bgBlue: Chalk,
    readonly bgMagenta: Chalk,
    readonly bgCyan: Chalk,
    readonly bgWhite: Chalk,
    readonly bgGray: Chalk,
    readonly bgGrey: Chalk,
    readonly bgBlackBright: Chalk,
    readonly bgRedBright: Chalk,
    readonly bgGreenBright: Chalk,
    readonly bgYellowBright: Chalk,
    readonly bgBlueBright: Chalk,
    readonly bgMagentaBright: Chalk,
    readonly bgCyanBright: Chalk,
    readonly bgWhiteBright: Chalk,
  }

  declare interface ChalkModule extends Chalk {
    supportsColor: ColorSupport | false;
    stderr: Chalk & {supportsColor: ColorSupport | false, ...};
  }

  declare module.exports: ChalkModule;
}
