/// <reference types="node" />
import * as stream from 'stream';
/**
 * Interface for exec options
 */
export interface ExecOptions {
    /** optional working directory.  defaults to current */
    cwd?: string;
    /** optional envvar dictionary.  defaults to current process's env */
    env?: {
        [key: string]: string;
    };
    /** optional.  defaults to false */
    silent?: boolean;
    /** optional out stream to use. Defaults to process.stdout */
    outStream?: stream.Writable;
    /** optional err stream to use. Defaults to process.stderr */
    errStream?: stream.Writable;
    /** optional. whether to skip quoting/escaping arguments if needed.  defaults to false. */
    windowsVerbatimArguments?: boolean;
    /** optional.  whether to fail if output to stderr.  defaults to false */
    failOnStdErr?: boolean;
    /** optional.  defaults to failing on non zero.  ignore will not fail leaving it up to the caller */
    ignoreReturnCode?: boolean;
    /** optional. How long in ms to wait for STDIO streams to close after the exit event of the process before terminating. defaults to 10000 */
    delay?: number;
    /** optional. Listeners for output. Callback functions that will be called on these events */
    listeners?: {
        stdout?: (data: Buffer) => void;
        stderr?: (data: Buffer) => void;
        stdline?: (data: string) => void;
        errline?: (data: string) => void;
        debug?: (data: string) => void;
    };
}
