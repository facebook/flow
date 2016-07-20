import type {Stats} from 'fs';
declare module 'sane' {
  declare type Sane$Options = {
    glob?: Array<string>,
    poll?: boolean,
    watchman?: boolean,
    dot?: boolean,
  };
  declare class Sane$Watcher {
    on(event: 'ready', callback: () => mixed): void;
    on(event: 'change', callback: (filepath: string, root: string, stat: Stats) => mixed): void;
    on(event: 'add', callback: (filepath: string, root: string, stat: Stats) => mixed): void;
    on(event: 'delete', callback: (filepath: string, root: string) => mixed): void;
  }
  declare module.exports: (path: string, options?: Sane$Options) => Sane$Watcher;
}
