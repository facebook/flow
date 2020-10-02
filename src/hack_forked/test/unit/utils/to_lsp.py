#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Takes raw LSP payloads and converts them into HTTP.

This script is useful for testing the LSP server: you can feed in a payload and
confirm that the responses are correct.

Input format: the first word is the LSP method (see the LSP spec for a full
listing). The rest of the line is the JSON payload. You can also comment out a
line by prepending with a `#` or sleep for a specified amount of time using
`sleep`. For example:

    initialize {"initializationOptions":{},"processId":null,"rootPath":"/data/users/waleedk/test","capabilities":{}}
    textDocument/completion {"filename": "/data/users/waleedk/test/foo.php","position": {"line": 8, "column": 1}}
    # textDocument/completion {"filename": "/data/users/waleedk/test/foo.php","position": {"line": 8, "column": 2}}
    sleep 3

Suggested command-line usage:

    python to_lsp.py filename.txt | hh lsp

Note that this script keeps its stdout open, and does not terminate. That is
because a closed pipe is an error for the language server. You can terminate
the connection explicitly with control-C.
"""
import fileinput
import sys
import time


def main():
    for line in fileinput.input():
        command = to_http(line)
        if command:
            sys.stdout.write(to_http(line))
            sys.stdout.flush()

    # Avoid closing stdin, because that is an error for the LSP server.
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        pass


def to_http(line):
    method, line = line.strip().split(" ", 1)
    if method == "#":
        return None
    elif method == "sleep":
        seconds = int(line)
        time.sleep(seconds)
        return None

    json_rpc_payload = """
{{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "{method}",
    "params": {params}
}}
    """.strip().format(
        method=method, params=line
    )
    content_length = len(json_rpc_payload)
    return """
Content-Length: {}\r
\r
{}
""".strip().format(
        content_length, json_rpc_payload
    )


if __name__ == "__main__":
    main()
