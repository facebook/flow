declare namespace AccessorNS {
  declare class Item {
    get id(): number;
    set id(value: number): void;
    get label(): string;
    static get defaultLabel(): string;
  }
}

export {AccessorNS};
