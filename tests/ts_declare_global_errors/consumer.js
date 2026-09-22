validGlobal as string;
InlineImport as string; // ERROR
nestedGlobal as string; // ERROR
scriptGlobal as string; // ERROR
javascriptGlobal as string; // ERROR
ambientGlobal as string; // ERROR

declare const imported: FromImport;
declare const aliased: FromAlias;
declare const fromValue: FromValue;

imported.value as string;
aliased.value as string;
fromValue as number;
