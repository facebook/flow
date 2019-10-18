(*
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* File urls: https://tools.ietf.org/html/rfc8089                             *)
(* Related definitions: https://tools.ietf.org/html/rfc3986                   *)
(* Notes on UNC file urls and edge-cases:                                     *)
(* https://foswiki.org/Support/Faq72                                          *)
(* https://blogs.msdn.microsoft.com/ie/2006/12/06/file-uris-in-windows/       *)
(* https://msdn.microsoft.com/en-us/library/windows/desktop/ff819129(v=vs.85).aspx *)

(* unescape: given a string with %-escapes like "foo%4Abar", unescapes it.    *)
(* Throws if there are incorrect %-escapes (not followed by two hex digits)   *)
(* and for %-escapes that are outside 7-bit printable ascii.                  *)
let percent_re = Str.regexp {|%\([0-9a-fA-F]?[0-9a-fA-F]?\)|}

let unescape (s : string) : string =
  let subst _ =
    let hex = Str.matched_group 1 s in
    if String.length hex <> 2 then failwith ("incorrect %-escape in " ^ s);
    let code = int_of_string ("0x" ^ hex) in
    if code < 32 || code > 127 then failwith ("only 7bit ascii allowed in " ^ s);
    String.make 1 (Char.chr code)
  in
  Str.global_substitute percent_re subst s

(* escape: given a string, uses %-escaping for all but the safe chars.        *)
(* Throws if asked to escape something outside 7-bit printable ascii.         *)
let escape ~(safe_chars : string) (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  let f (c : char) : unit =
    if String.contains safe_chars c then
      Buffer.add_char buf c
    else
      let code = Char.code c in
      if code < 32 || code > 127 then
        failwith ("only 7bit ascii allowed in " ^ s);
      Buffer.add_string buf (Printf.sprintf "%%%02X" code)
  in
  String.iter f s;
  Buffer.contents buf

(* parse: turns a file url into an absolute path. It will turn a unix-style   *)
(* url "file://localhost/path" into "/path", and will turn a dos-style url    *)
(* "file://localhost/C|/path" into "C:/path". It doesn't try to turn forward  *)
(* slashes into dos-style backslashes, since win32 can happily consume both.  *)
(* It rejects unc urls that use five-slash encoding "file://///server/path".  *)
(* The host can be either "localhost" or empty. The function will unescape    *)
(* %-encoding, but throws if that was used to encode something outside 7-bit  *)
(* ascii.  This function doesn't attempt to validate the escaping of the url: *)
(* doesn't complain if the uri has %-encoding where it wasn't needed, nor if  *)
(* the uri fails to %-encode where it should.                                 *)
let dos_url_re =
  (* e.g. c:\ or z|/ *)
  Str.regexp {|^\([a-zA-Z]\)[:|]\([/\].*\)$|}

let url_re = Str.regexp {|^file://\([^/?#]*\)/\([^?#]*\)\(.*\)$|}

let parse (uri : string) : string =
  if not (Str.string_match url_re uri 0) then
    failwith ("not a file url - " ^ uri);
  let host = Str.matched_group 1 uri in
  let path = Str.matched_group 2 uri in
  let query_fragment = Str.matched_group 3 uri in
  let path = unescape path in
  (* this uses regexp internally *)
  if host <> "" && host <> "localhost" then failwith ("not localhost - " ^ uri);
  if query_fragment <> "" then
    failwith ("file url can't have query/fragment - " ^ uri);
  if Str.string_match dos_url_re path 0 then
    let drive_letter = Str.matched_group 1 path in
    let rest = Str.matched_group 2 path in
    drive_letter ^ ":" ^ rest
  else if String.length path > 0 && path.[0] = '/' then
    failwith ("UNC file urls not supported - " ^ uri)
  else
    "/" ^ path

(* create: turns an absolute path into a file uri. The absolute path must be  *)
(* either unix-style absolute path "/path" or dos-style "c:\path" (in which   *)
(* case it treats both forward- and back-slashes as path separators, and      *)
(* converts both to forward-slash, as per URL rules). It rejects unc-style    *)
(* paths "\\?\c:\path" or "\\server\share\path".                              *)
(* This function merely guarantees that valid absolute paths will give valid  *)
(* file URLs; it doesn't also validate that what's been given is a perfectly  *)
(* well-formed path. For instance, if the path has ? or * or : characters,    *)
(* it will accept them and render them as %3F and * and :. This function also *)
(* doesn't do escaping - e.g.  if given "he\\o" it treats this as a filename  *)
(* with two backslashes rather than one. It's therefore impossible to create  *)
(* a file url which has forward slashes as part of a file/directory-name; all *)
(* slashes will be interpreted as path separators.                            *)
let dos_re = Str.regexp {|^\([a-zA-Z]\):\([/\].*\)$|} (* e.g. c:\ or z:/ *)

let backslash_re = Str.regexp {|\\|} (* matches a single backslash *)

let path_safe_chars =
  "/-._~!$&'()*+,;=@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let create (path : string) : string =
  let absolute_path =
    if Str.string_match dos_re path 0 then
      let drive_letter = Str.matched_group 1 path in
      let rest = Str.matched_group 2 path in
      Printf.sprintf
        "%s:%s"
        drive_letter
        (Str.global_replace backslash_re "/" rest)
    else if String_utils.string_starts_with path "/" then
      String_utils.lstrip path "/"
    else
      failwith ("Not an absolute filepath - " ^ path)
  in
  "file:///" ^ escape path_safe_chars absolute_path
