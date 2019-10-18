/// <reference types="node" />
import * as fs from 'fs';
export declare const chmod: typeof fs.promises.chmod, copyFile: typeof fs.promises.copyFile, lstat: typeof fs.promises.lstat, mkdir: typeof fs.promises.mkdir, readdir: typeof fs.promises.readdir, readlink: typeof fs.promises.readlink, rename: typeof fs.promises.rename, rmdir: typeof fs.promises.rmdir, stat: typeof fs.promises.stat, symlink: typeof fs.promises.symlink, unlink: typeof fs.promises.unlink;
export declare const IS_WINDOWS: boolean;
export declare function exists(fsPath: string): Promise<boolean>;
export declare function isDirectory(fsPath: string, useStat?: boolean): Promise<boolean>;
/**
 * On OSX/Linux, true if path starts with '/'. On Windows, true for paths like:
 * \, \hello, \\hello\share, C:, and C:\hello (and corresponding alternate separator cases).
 */
export declare function isRooted(p: string): boolean;
/**
 * Recursively create a directory at `fsPath`.
 *
 * This implementation is optimistic, meaning it attempts to create the full
 * path first, and backs up the path stack from there.
 *
 * @param fsPath The path to create
 * @param maxDepth The maximum recursion depth
 * @param depth The current recursion depth
 */
export declare function mkdirP(fsPath: string, maxDepth?: number, depth?: number): Promise<void>;
/**
 * Best effort attempt to determine whether a file exists and is executable.
 * @param filePath    file path to check
 * @param extensions  additional file extensions to try
 * @return if file exists and is executable, returns the file path. otherwise empty string.
 */
export declare function tryGetExecutablePath(filePath: string, extensions: string[]): Promise<string>;
