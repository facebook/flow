# Hack editor API

## Establishing a persistent connection to the server

Running:

  hh_client ide <path>

Where `<path>` is a directory (or descendant of directory) containing `.hhconfig` file will launch a persistent client that you can communicate with via standard input / output. If the server for this directory is not running, you need to start it first with `hh_client start <path>`. The server supports only having one persistent client, and if there is already a client connected, it will be disconnected (and exit with non-zero exit code).

## Transport layer

All the the messages sent / received should be ASCII encoded, single line, JSON strings. The newline character is used to separate individual messages. All of the JSON fields that deserialize to string type, should deserialize to ASCII strings - there is no support for Unicode file names or Unicode source files.

## RPC protocol

The messages should conform to [JSON-RPC 2.0](http://www.jsonrpc.org/specification) specification. Messages are processed serially in arrival order. For example, “infer type at position in file” will use file contents established by most recent preceding “open file” / “change file” call.

## Common structures

JSON structures shared by more than single request / response / notification.

`string:`

All of the string typed members are intended only to be displayed in editor - they should not be parsed to extract additional information from them, as there is no guarantees about their structure or contents.

`Position:`

  {
    /**
     * 1-based line number
     */
    line : integer;
    /**
     * 1-based column number
     */
    column : integer;
  }

`Range:`

  {
    /**
     * Range start position (inclusive)
     */
    start : Position;
    /**
     * Range end position (exclusive)
     */
    end : Position;
  }

`FilePosition:`

  {
    /**
     * Absolute path of a file
     */
    filename : string;
    /**
     * Position inside this file
     */
    position : Position;
  }

## Requests and notifications

### Initialization request

This request **must** be the first request sent by the client after establishing the connection, before any other requests.

*Client request:*

  method : "init"
  params : InitParams

where `InitParams` is defined as:

  {
    /**
     * Arbitrary name identifying the client, provided for
     * statistics purposes. Example: "Nuclide 0.182"
     */
    client_name : string;
    /**
     * The version of this API that client supports.
     */
    client_api_version : integer;
  }

*Server response:*

  {
    /**
     * The most recent version of this API that server supports.
     */
    server_api_version : integer;
  }

`client_api_version` is just a suggestion for the server to ease introducing breaking changes: the server might try to match the behavior of `client_api_version` < `server_api_version` version of the API, but it's not required to do so in all of the cases - the decision is left to the server on per-feature basis, subject to individual judgment on how long do we want to wait before stopping support for older versions / features. Move fast!

### File synchronization

Hack server watches all disk files in a project and uses their contents as a source of truth. But the files open in editor might have changes that are not saved yet - using disk versions for those files could lead to inconsistent / outdated results. To avoid this, editor can use file synchronization commands described in this section to always provide the server with most recent version of the file. It's still valid to issue commands referring to files that were not synchronized, but there are no hard guarantees about when their disk contents are reflected in server state when they change on disk, so it can lead to race conditions.

#### Open file notification

Notifies the server about opening the file in editor. From now on, server will ignore disk changes to this file.

*Client notification:*

  method : "didOpenFile"
  params : DidOpenFileParams

where `DidOpenFileParams` is defined as:

  {
    /**
     * Absolute path of the file that was opened.
     */
    filename : string;
    /**
     * Contents of opened file.
     */
    text : string;
  }

#### Change file notification

Notifies the server about change to a file that was previously opened in editor.

*Client notification:*

  method : "didChangeFile"
  params : DidChangeFileParams

where `DidChangeFileParams` is defined as:

  {
    /**
     * Absolute path of the file that was changed.
     */
    filename : string;
    /**
     * Changes to file contents. In case of multiple changes, they are
     * applied to the file serially in the order they appear in the
     * list.
     */
    changes : TextEdit[];
  }

`TextEdit:`

  {
    /**
     * The range of the file to be replaced by text.
     * If omitted, entire file is replaced.
     */
    range? : Range;
    /**
     * New contents to be inserted. Set to empty string to represent
     * deletions.
     */
    text : string;
  }

#### Close file notification

Notifies the server that a previously opened file was closed in editor. The server will now use disk contents as source of truth for this file.

*Client notification:*

  method : "didCloseFile"
  params : DidCloseParams

where `DidCloseParams` is defined as:

  {
    /**
     * Absolute path of the file that was closed.
     */
    filename : string;
  }

#### Diagnostics notification

The server will analyze the files on the disk and modified via file synchronization and notify the client about found issues using this notification.

*Server notification:*

  method : “diagnostics”
  params : DiagnosticsParams

where DiagnosticsParams is defined as:

  {
    filename : string;
    errors : Error[];
  }

`Error:`

  ErrorMessage[]

`ErrorMessage:`

  {
    message : string;
    position : FilePosition;
  }

### Autocomplete request

*Client request:*

    method : "autocomplete"
    params : FilePosition

*Server response:*

  AutocompleteItem[]

`AutocompleteItem:`

  {
    /**
     * The text to be inserted when selecting this completion item.
     *
     * The text includes the prefix which is already typed out in the
     * file. For example:
     *
     * $x = new Fo<autocomplete here>
     *
     * can include "Foo" as text to complete.
     */
    text : string;

    /**
     * Additional information about completion type, e.g:
     * "abstract class", "function"
     */
    type : string;

    /**
     * For callable completions (functions, methods), additional
     * information about it.
     */
    callable_details? : {
      /**
       * Callable return type.
       */
      return_type : string;
      /**
       * Informations about all of the callable parameters.
       */
      params: CallableParam[]
    }  
  }

`CallableParam:`

  {
    /**
     * Argument name, as specified in callable declaration.
     */
    name : string;
    /**
     * Expected type of callable argument.
     */
    type : string;
  }
