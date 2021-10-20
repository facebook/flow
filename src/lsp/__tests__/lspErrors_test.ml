(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

type error = {
  uri: Lsp.DocumentUri.t;
  kind: string;
  msg: string;
}

let mk_clear_error uri = { uri; kind = "<FAKE ERROR>"; msg = "Errors cleared for file" }

(* Build a mock lsp diagnostic *)
let mk_diagnostic { uri = _; kind; msg } =
  Lsp.(
    PublishDiagnostics.
      {
        range = { start = { line = 10; character = 20 }; end_ = { line = 10; character = 30 } };
        severity = Some Lsp.PublishDiagnostics.Error;
        code = StringCode kind;
        source = Some "Flow";
        message = msg;
        relatedInformation = [];
        relatedLocations = [];
      }
    
  )

(* Take the json output and convert it back into a list of errors *)
let error_list_of_json_response json =
  Hh_json.(
    match json with
    | JSON_Object
        [
          ("jsonrpc", JSON_String "2.0");
          ("method", JSON_String "textDocument/publishDiagnostics");
          ("params", JSON_Object [("uri", JSON_String uri); ("diagnostics", JSON_Array diagnostics)]);
        ] ->
      let uri = Lsp.DocumentUri.of_string uri in
      (match diagnostics with
      | [] -> [mk_clear_error uri]
      | _ ->
        List.map
          (function
            | JSON_Object
                [
                  ("range", _);
                  ("severity", _);
                  ("code", JSON_String kind);
                  ("source", JSON_String "Flow");
                  ("message", JSON_String msg);
                  ("relatedInformation", _);
                  ("relatedLocations", _);
                ] ->
              { uri; kind; msg }
            | _ -> assert_failure "Diagnostic JSON doesn't match expected format")
          diagnostics)
    | _ -> assert_failure "JSON output doesn't match expected format"
  )

(* Pretty print a list of errors *)
let printer errors =
  errors
  |> List.map (fun { uri; kind; msg } ->
         let uri = Lsp.DocumentUri.to_string uri in
         Printf.sprintf "    %s: %s: %s," uri kind msg
     )
  |> String.concat "\n"
  |> Printf.sprintf "\n[\n%s\n]"

(* Wraps some lspErrors calls and records what json is sent. Then asserts that all the expected
 * errors were sent *)
let with_assert_errors_match ~ctxt ~reason ~expected f =
  let actual = ref [] in
  let ret = f (fun json -> actual := List.rev_append (error_list_of_json_response json) !actual) in
  let actual = List.rev !actual in
  let sort = List.sort Stdlib.compare in
  assert_equal ~ctxt ~printer ~msg:reason (sort expected) (sort actual);
  ret

(* Assert that NO json is sent. This is different than asserting that we sent a list of 0 errors *)
let assert_no_send ~reason _ =
  assert_failure (Printf.sprintf "Expected no send, but got a send for %S" reason)

(* Given an error list, group it by uri and convert to diagnostics *)
let map_of_error_list error_list =
  List.fold_right
    (fun error map ->
      let existing =
        match Lsp.UriMap.find_opt error.uri map with
        | None -> []
        | Some existing -> existing
      in
      Lsp.UriMap.add error.uri (mk_diagnostic error :: existing) map)
    error_list
    Lsp.UriMap.empty

let path_to_foo = Lsp.DocumentUri.of_string "file:///path/to/foo.js"

let path_to_bar = Lsp.DocumentUri.of_string "file:///path/to/bar.js"

let foo_infer_error_1 = { uri = path_to_foo; kind = "InferError"; msg = "Your code is broken 1" }

let foo_infer_error_2 = { uri = path_to_foo; kind = "InferError"; msg = "Your code is broken 2" }

let foo_parse_error_1 = { uri = path_to_foo; kind = "ParseError"; msg = "Your code no parse 1" }

let foo_parse_error_2 = { uri = path_to_foo; kind = "ParseError"; msg = "Your code no parse 2" }

let bar_infer_error_1 = { uri = path_to_bar; kind = "InferError"; msg = "Your code is broken 1" }

let bar_infer_error_2 = { uri = path_to_bar; kind = "InferError"; msg = "Your code is broken 2" }

let bar_parse_error_1 = { uri = path_to_bar; kind = "ParseError"; msg = "Your code no parse 1" }

let bar_parse_error_2 = { uri = path_to_bar; kind = "ParseError"; msg = "Your code no parse 2" }

let clearing_errors_from_empty_is_a_no_op _ctxt =
  let reason = "Clearing all the errors when there are no errors should not send anything" in
  LspErrors.empty |> LspErrors.clear_all_errors_and_send (assert_no_send ~reason) |> ignore

let clear_all_live_errors_and_send ctxt =
  let reason =
    "Clearing the live errors from a file with no live errors should not send anything"
  in
  let errors =
    LspErrors.empty |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  let reason = "Setting the live parse errors for foo.js to 0 errors won't trigger a send" in
  let errors =
    errors |> LspErrors.set_live_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason = "Setting server errors to 0 streamed errors should not send anything" in
  let errors =
    errors
    |> LspErrors.add_streamed_server_errors_and_send
         (assert_no_send ~reason)
         (Lsp.UriMap.add path_to_foo [] Lsp.UriMap.empty)
  in
  let reason = "Clearing the live errors from a file with 0 streamed errors should not send" in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  let reason = "Setting the live parse errors for foo.js to 0 errors again should trigger a send" in
  let errors =
    errors |> LspErrors.set_live_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason = "Setting server errors to be 1 streamed non-parse error should send that error" in
  let expected = [foo_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Clearing the live errors from a file with 0 streamed parse errors should not send"
  in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  let reason =
    "Setting the live parse errors for foo.js to 0 errors one more time should trigger a send"
  in
  let errors =
    errors |> LspErrors.set_live_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason =
    "Adding a parse error to server errors should not trigger a send since we have live parse "
    ^ "errors"
  in
  let to_send = [foo_parse_error_1] in
  let errors =
    errors
    |> LspErrors.add_streamed_server_errors_and_send
         (assert_no_send ~reason)
         (map_of_error_list to_send)
  in
  let reason =
    "Clearing the live errors from a file with 1 streamed parse errors will trigger a send"
  in
  let expected = [foo_parse_error_1; foo_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_live_errors_and_send send path_to_foo
    )
  in
  ignore errors

let finalized_errors_in_isolation ctxt =
  let reason =
    "Setting the finalized server errors to empty when there already were no errors should "
    ^ "not send anything"
  in
  let errors =
    LspErrors.empty
    |> LspErrors.set_finalized_server_errors_and_send (assert_no_send ~reason) (map_of_error_list [])
  in
  let reason =
    "Setting finalized server errors when there were no errors before should send all the "
    ^ "finalized errors"
  in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason = "Setting the exact same finalized server errors again will resend all errors" in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting the finalized server errors to be 0 errors will clear errors for all files"
  in
  let expected = [mk_clear_error path_to_foo; mk_clear_error path_to_bar] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list [])
    )
  in
  let reason = "Putting an error in each file again" in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting the finalized server errors to only include foo.js will clear the errors for bar.js"
  in
  let expected = [foo_infer_error_1; mk_clear_error path_to_bar] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_finalized_server_errors_and_send
             send
             (map_of_error_list [foo_infer_error_1])
    )
  in
  let reason = "Putting an error in each file again" in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason = "Clearing all errors will clear errors for all files" in
  let expected = [mk_clear_error path_to_foo; mk_clear_error path_to_bar] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_errors_and_send send
    )
  in
  ignore errors

let streamed_errors_in_isolation ctxt =
  let reason =
    "Streaming in 0 server errors when there were no errors before will not do anything"
  in
  let errors =
    LspErrors.empty
    |> LspErrors.add_streamed_server_errors_and_send (assert_no_send ~reason) (map_of_error_list [])
  in
  let reason =
    "Streaming in server errors when there were no errors before should send all the "
    ^ "newly streamed errors"
  in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Streaming in the same server errors again will again resend the errors for those files, "
    ^ "but now each error will appear twice"
  in
  let expected = [foo_infer_error_1; foo_infer_error_1; bar_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.add_streamed_server_errors_and_send
             send
             (map_of_error_list [foo_infer_error_1; bar_infer_error_1])
    )
  in
  let reason = "Streaming in 1 more error for foo.js will cause all of foo's errors to be resent" in
  let expected = [foo_infer_error_1; foo_infer_error_1; foo_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list [foo_infer_error_2])
    )
  in
  let reason = "Streaming in 0 errors will not trigger a send" in
  let errors =
    errors
    |> LspErrors.add_streamed_server_errors_and_send
         (assert_no_send ~reason)
         (Lsp.UriMap.add path_to_foo [] Lsp.UriMap.empty)
  in
  let reason = "Clearing all errors will clear errors for all files" in
  let expected = [mk_clear_error path_to_foo; mk_clear_error path_to_bar] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_errors_and_send send
    )
  in
  ignore errors

let streamed_and_finalized_server_errors ctxt =
  let reason =
    "Setting finalized server errors when there were no errors before should send all the "
    ^ "finalized errors"
  in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        LspErrors.empty
        |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Sending streamed errors for foo.js should replace the finalized errors for that file"
  in
  let expected = [foo_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Sending finalized errors for foo.js should replace the streamed errors and the old "
    ^ "finalized errors"
  in
  let expected = [bar_infer_error_2; mk_clear_error path_to_foo] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_finalized_server_errors_and_send
             send
             (map_of_error_list [bar_infer_error_2])
    )
  in
  ignore errors

let live_parse_errors_override_finalized_errors ctxt =
  let reason =
    "Setting parse errors to 0 parse errors when there are no server errors doesn't trigger a "
    ^ "send"
  in
  let errors =
    LspErrors.empty
    |> LspErrors.set_live_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason =
    "Setting finalized server errors when there were no errors before should send all the "
    ^ "finalized errors"
  in
  let expected = [foo_infer_error_1; foo_parse_error_1; bar_infer_error_1; bar_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting live parse errors for foo.js will replace the known parse errors for that file"
  in
  let expected = [foo_infer_error_1; foo_parse_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_parse_errors_and_send
             send
             path_to_foo
             [mk_diagnostic foo_parse_error_2]
    )
  in
  let reason = "Setting finalized server errors will still use the live parse errors" in
  let to_set = [foo_infer_error_2; foo_parse_error_1; bar_infer_error_1; bar_parse_error_1] in
  let expected = [foo_infer_error_2; foo_parse_error_2; bar_infer_error_1; bar_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list to_set)
    )
  in
  let reason =
    "Clearing the live parse errors for foo.js will resend the server errors for that file"
  in
  let expected = [foo_infer_error_2; foo_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_live_errors_and_send send path_to_foo
    )
  in
  let reason = "Clearing live errors again won't do anything" in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  let reason =
    "Setting finalized server errors to only non-parse errors for foo.js and some parse errors "
    ^ "for bar.js"
  in
  let expected = [foo_infer_error_1; bar_infer_error_1; bar_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting live parse errors for foo.js to [] won't trigger a send since there are no server "
    ^ "parse errors for foo.js"
  in
  let errors =
    errors |> LspErrors.set_live_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason =
    "Setting live parse errors for bar.js to [] will trigger a send since there are server "
    ^ "parse errors for bar.js"
  in
  let expected = [bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_live_parse_errors_and_send send path_to_bar []
    )
  in
  ignore errors

let live_parse_errors_override_streamed_errors ctxt =
  let reason =
    "Setting streamed server errors when there were no errors before should send all the "
    ^ "streamed errors"
  in
  let expected = [foo_infer_error_1; foo_parse_error_1; bar_infer_error_1; bar_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        LspErrors.empty
        |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting live parse errors for foo.js will replace the known parse errors for that file"
  in
  let expected = [foo_infer_error_1; foo_parse_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_parse_errors_and_send
             send
             path_to_foo
             [mk_diagnostic foo_parse_error_2]
    )
  in
  let reason = "Streaming in parse and type errors will ignore the parse errors for now" in
  let to_send = [foo_infer_error_2; foo_parse_error_1] in
  let expected = [foo_infer_error_1; foo_infer_error_2; foo_parse_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list to_send)
    )
  in
  let reason =
    "Clearing the live parse errors for foo.js will resend the server errors for that file"
  in
  let expected = [foo_infer_error_1; foo_infer_error_2; foo_parse_error_1; foo_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_live_errors_and_send send path_to_foo
    )
  in
  let reason = "Clearing live errors again won't do anything" in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  ignore errors

let live_non_parse_errors_override_finalized_errors ctxt =
  let reason =
    "Setting live errors to 0 live errors when there are no server errors doesn't trigger a "
    ^ "send"
  in
  let errors =
    LspErrors.empty
    |> LspErrors.set_live_non_parse_errors_and_send (assert_no_send ~reason) path_to_foo []
  in
  let reason =
    "Setting finalized server errors when there were no errors before should send all the "
    ^ "finalized errors"
  in
  let expected = [foo_infer_error_1; bar_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting live non_parse errors for foo.js will replace the server non_parse errors for "
    ^ "that file"
  in
  let to_send = [foo_parse_error_1; foo_infer_error_2] in
  let expected = [foo_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_non_parse_errors_and_send
             send
             path_to_foo
             (List.map mk_diagnostic to_send)
    )
  in
  let reason =
    "Setting finalized server errors will still use the live errors for foo.js and the server "
    ^ "errors for bar.js. But since the live errors aren't changing, we won't resend errors for "
    ^ "foo.js"
  in
  let to_set = [foo_infer_error_1; bar_infer_error_2] in
  let expected = [bar_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_finalized_server_errors_and_send send (map_of_error_list to_set)
    )
  in
  let reason = "Clearing the live errors for foo.js will resend the server errors for that file" in
  let expected = [foo_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_live_errors_and_send send path_to_foo
    )
  in
  let reason = "Clearing live errors again won't do anything" in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  ignore errors

let live_non_parse_errors_override_streamed_errors ctxt =
  let reason =
    "Setting streamed server errors when there were no errors before should send all the "
    ^ "streamed errors"
  in
  let expected = [foo_infer_error_1; foo_parse_error_1; bar_infer_error_1; bar_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        LspErrors.empty
        |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list expected)
    )
  in
  let reason =
    "Setting live non_parse errors for foo.js will replace the server non_parse errors for "
    ^ "that file"
  in
  let to_send = [foo_parse_error_2; foo_infer_error_2] in
  let expected = [foo_parse_error_1; foo_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_non_parse_errors_and_send
             send
             path_to_foo
             (List.map mk_diagnostic to_send)
    )
  in
  let reason = "Streaming in more non_parse errors for foo.js will not trigger a send" in
  let to_send = [foo_infer_error_1] in
  let errors =
    errors
    |> LspErrors.add_streamed_server_errors_and_send
         (assert_no_send ~reason)
         (map_of_error_list to_send)
  in
  let reason = "Streaming in a parse error for foo.js will trigger a send" in
  let to_send = [foo_parse_error_1] in
  let expected = [foo_infer_error_2; foo_parse_error_1; foo_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.add_streamed_server_errors_and_send send (map_of_error_list to_send)
    )
  in
  let reason =
    "Clearing the live parse errors for foo.js will resend the server errors for that file"
  in
  let expected = [foo_infer_error_1; foo_infer_error_1; foo_parse_error_1; foo_parse_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.clear_all_live_errors_and_send send path_to_foo
    )
  in
  let reason = "Clearing live errors again won't do anything" in
  let errors =
    errors |> LspErrors.clear_all_live_errors_and_send (assert_no_send ~reason) path_to_foo
  in
  ignore errors

let live_parse_errors_and_live_non_parse_errors ctxt =
  let reason =
    "Setting live non-parse errors for foo.js will replace all the server errors for that file "
    ^ "and filter out parse errors"
  in
  let to_send = [foo_infer_error_1; foo_parse_error_1] in
  let expected = [foo_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        LspErrors.empty
        |> LspErrors.set_live_non_parse_errors_and_send
             send
             path_to_foo
             (List.map mk_diagnostic to_send)
    )
  in
  let reason =
    "Setting live parse errors for foo.js will add to the live non-parse errors and filter out "
    ^ "non-parse errors"
  in
  let to_send = [foo_infer_error_2; foo_parse_error_2] in
  let expected = [foo_infer_error_1; foo_parse_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_parse_errors_and_send send path_to_foo (List.map mk_diagnostic to_send)
    )
  in
  let reason =
    "Setting 0 live parse errors for foo.js will strip out the parse error that live errors shows"
  in
  let expected = [foo_infer_error_1] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors |> LspErrors.set_live_parse_errors_and_send send path_to_foo []
    )
  in
  let reason = "Setting live errors for foo.js will not affect the live parse errors shown" in
  let to_send = [foo_infer_error_2; foo_parse_error_2] in
  let expected = [foo_infer_error_2] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_non_parse_errors_and_send
             send
             path_to_foo
             (List.map mk_diagnostic to_send)
    )
  in
  let reason =
    "Setting live errors for foo.js with ONLY parse errors when there are 0 live parse errors "
    ^ "will clear the file"
  in
  let to_send = [foo_parse_error_2] in
  let expected = [mk_clear_error path_to_foo] in
  let errors =
    with_assert_errors_match ~ctxt ~reason ~expected (fun send ->
        errors
        |> LspErrors.set_live_non_parse_errors_and_send
             send
             path_to_foo
             (List.map mk_diagnostic to_send)
    )
  in
  let reason =
    "Setting live errors for foo.js with ONLY parse errors should NOT trigger a send if modulo "
    ^ "parse errors there are 0 live errors before and 0 live errors after"
  in
  let to_send = [foo_parse_error_2] in
  let errors =
    errors
    |> LspErrors.set_live_non_parse_errors_and_send
         (assert_no_send ~reason)
         path_to_foo
         (List.map mk_diagnostic to_send)
  in
  ignore errors

let tests =
  "LwtErrors"
  >::: [
         "clearing_errors_from_empty_is_a_no_op" >:: clearing_errors_from_empty_is_a_no_op;
         "clear_all_live_errors_and_send" >:: clear_all_live_errors_and_send;
         "finalized_errors_in_isolation" >:: finalized_errors_in_isolation;
         "streamed_errors_in_isolation" >:: streamed_errors_in_isolation;
         "streamed_and_finalized_server_errors" >:: streamed_and_finalized_server_errors;
         "live_parse_errors_override_finalized_errors"
         >:: live_parse_errors_override_finalized_errors;
         "live_parse_errors_override_streamed_errors" >:: live_parse_errors_override_streamed_errors;
         "live_non_parse_errors_override_finalized_errors"
         >:: live_non_parse_errors_override_finalized_errors;
         "live_non_parse_errors_override_streamed_errors"
         >:: live_non_parse_errors_override_streamed_errors;
         "live_parse_errors_and_live_non_parse_errors"
         >:: live_parse_errors_and_live_non_parse_errors;
       ]

let () = run_test_tt_main tests
