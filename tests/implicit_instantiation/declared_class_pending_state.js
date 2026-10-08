type Settings = {imports?: Object, ...};

declare class Service {
  uniqueId(prefix?: ?string): string;
  settings: Settings;
}

declare module.exports: Service;
