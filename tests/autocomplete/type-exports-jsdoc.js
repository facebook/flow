//@flow

/** this is ExportFoo */
export type ExportFoo = number;
/** this is ExportBar */
export type ExportBar<T> = ?T;
/** this is ExportClass */
export class ExportClass {};
/** this is ExportInterace */
export interface ExportInterface {};
/** this is ExportEnum */
export enum ExportEnum { Inl, Inr }
/** this is ExportValue */
export const exportValue = 1;
