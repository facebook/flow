declare type FileSystemHandleKind = "file" | "directory";

// https://wicg.github.io/file-system-access/#api-filesystemhandle
declare interface FileSystemHandle {
  +kind: FileSystemHandleKind;
  +name: string;

  isSameEntry: (other: FileSystemHandle) => Promise<boolean>;
  queryPermission?: (
    descriptor: FileSystemHandlePermissionDescriptor
  ) => Promise<PermissionStatus>;
  requestPermission?: (
    descriptor: FileSystemHandlePermissionDescriptor
  ) => Promise<PermissionStatus>;
}

// https://fs.spec.whatwg.org/#api-filesystemfilehandle
declare class FileSystemFileHandle implements FileSystemHandle {
  +kind: "file";
  constructor(name: string): void;

  getFile(): Promise<File>;
  createSyncAccessHandle(): Promise<FileSystemSyncAccessHandle>;
  createWritable(options?: {
    keepExistingData?: boolean,
  }): Promise<FileSystemWritableFileStream>;
}

// https://fs.spec.whatwg.org/#api-filesystemdirectoryhandle
declare class FileSystemDirectoryHandle implements FileSystemHandle {
  +kind: "directory";
  constructor(name: string): void;

  getDirectoryHandle(name: string, options?: {create?: boolean}): Promise<FileSystemDirectoryHandle>;
  getFileHandle(name: string, options?: {create?: boolean}): Promise<FileSystemFileHandle>;
  removeEntry(name: string, options?: {recursive?: boolean}): Promise<void>;
  resolve(possibleDescendant: FileSystemHandle): Promise<Array[string] | null>;

  // Async iterator functions
  @@asyncIterator(): AsyncIterator<[string, FileSystemHandle]>;
  entries(): AsyncIterator<[string, FileSystemHandle]>;
  keys(): AsyncIterator<string>;
  values(): AsyncIterator<FileSystemHandle>;
}

declare class FileSystemSyncAccessHandle {
  close(): void;
  flush(): void;
  getSize(): number;
  read(buffer: ArrayBuffer, options?: {at: number}): number;
  truncate(newSize: number): void;
  write(buffer: ArrayBuffer, options?: {at: number}): number;
}

declare class WriteableStreamDefaultWriter {
  +closed: Promise<void>;
  +desiredSize: number;
  +ready: Promise<void>;

  constructor(): void;

  abort(reason?: string): Promise<void>;
  close(): Promise<void>;
  releaseLock(): void;
  write(chunk: any): Promise<void>;

}

declare class WriteableStream {
  +locked: boolean;

  constructor(): void;
  abort(reason: string): Promise<string>;
  close(): Promise<void>;
  getWriter(): WriteableStreamDefaultWriter;
}

declare type FileSystemWriteableFileStreamDataTypes = ArrayBuffer | TypedArray | DataView | Blob | string;

declare type FileSystemWriteableFileStreamData = FileSystemWriteableFileStreamDataTypes | {
  type: "write",
  position?: number,
  data: FileSystemWriteableFileStreamDataTypes
} | {
  type: "seek",
  position: number,
  data: FileSystemWriteableFileStreamDataTypes
} | {
  type: "size",
  size: number
};

declare class FileSystemWritableFileStream extends WriteableStream {
  write(data: FileSystemWriteableFileStreamData): Promise<void>;
  truncate(size: number): Promise<void>;
  seek(position: number): Promise<void>;
}
