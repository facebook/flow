//@flow

declare opaque type FbtString: string;

declare type FbtElement = $FbtResultBase;

declare type FbtWithoutString = FbtString | FbtElement;

declare type Fbt = string | FbtWithoutString;
