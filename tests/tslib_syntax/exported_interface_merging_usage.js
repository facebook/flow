import type { Tippy, Control } from './exported_interface_merging';

// Merged interface members use the default type argument.
declare const tippy: Tippy;
tippy.a as number; // OK
tippy.b as string; // OK
tippy.c as boolean; // OK
tippy.a as string; // ERROR
tippy.b as number; // ERROR

// Merged generic interfaces accept explicit type arguments.
declare const generic: Tippy<string>;
generic.a as string; // OK
generic.b as string; // OK
generic.c as boolean; // OK
generic.a as number; // ERROR

// A single export specifier retains existing interface merging behavior.
declare const control: Control;
control.a as number; // OK
control.b as string; // OK
control.a as string; // ERROR
