export interface ElementLike {
  tagName: string;
}

export interface HTMLElementLike extends ElementLike {
  htmlOnly: number;
}

export declare var HTMLElementLike: {
  prototype: HTMLElementLike;
  new(): HTMLElementLike;
};

export declare var element: ElementLike;
export declare var unknownValue: unknown;

export interface InheritedHTMLElementLikeConstructor {
  prototype: HTMLElementLike;
  new(): HTMLElementLike;
}

export interface DerivedHTMLElementLikeConstructor
  extends InheritedHTMLElementLikeConstructor {}

export declare var DerivedHTMLElementLike: DerivedHTMLElementLikeConstructor;

export interface SpecializedHTMLElementLike extends HTMLElementLike {
  specialized: boolean;
}

export declare var OverloadedHTMLElementLike: {
  prototype: HTMLElementLike;
  new(kind: 'base'): HTMLElementLike;
  new(kind: 'specialized'): SpecializedHTMLElementLike;
};

export interface OtherLike {
  other: string;
}

export declare var unionValue: HTMLElementLike | OtherLike;

export declare var UnionReturningHTMLElementLike: {
  new(): HTMLElementLike | OtherLike;
};

export declare var overloadedValue: HTMLElementLike | SpecializedHTMLElementLike;
export declare var overloadedOrOther: HTMLElementLike | OtherLike;

export interface BoxLike<T> {
  value: T;
}

export declare var BoxLikeConstructor: {
  new <T>(value: T): BoxLike<T>;
};
