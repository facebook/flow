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

  // Async iterator functions
  @@asyncIterator(): AsyncIterator<[string, FileSystemHandle]>;
  entries(): AsyncIterator<[string, FileSystemHandle]>;
}

declare class FileSystemSyncAccessHandle {}

declare class FileSystemWritableFileStream {}
