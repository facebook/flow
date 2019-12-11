(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

let test_valid () =
  let examples =
    [
      ("file://localhost/etc/fstab", "/etc/fstab");
      ("file:///etc/fstab", "/etc/fstab");
      ("file://localhost/c:/WINDOWS/clock.avi", "c:/WINDOWS/clock.avi");
      ("file:///c:/WINDOWS/clock.avi", "c:/WINDOWS/clock.avi");
      ("file:///c%3A/WINDOWS/clock.avi", "c:/WINDOWS/clock.avi");
      ("file:///c|/WINDOWS/clock.avi", "c:/WINDOWS/clock.avi");
      ("file://localhost/path/to/the%20file.txt", "/path/to/the file.txt");
      ("file:///c:/path/to/the%20file.txt", "c:/path/to/the file.txt");
      ("file:///c|/path/to/the%20file.txt", "c:/path/to/the file.txt");
      ( "file:///u/lai/tik/tik76002/public_html/lerman.files/chaps",
        "/u/lai/tik/tik76002/public_html/lerman.files/chaps" );
      ("file:///etc/motd", "/etc/motd");
      ( "file:///c:/windows/My%20Documents%20100%2520/foo.txt",
        "c:/windows/My Documents 100%20/foo.txt" );
      ("file:///D:/Program%20Files/Viewer/startup.htm", "D:/Program Files/Viewer/startup.htm");
      ("file:///D:/Desktop/Book.pdf", "D:/Desktop/Book.pdf");
      ("file://localhost/D:/Desktop/", "D:/Desktop/");
      ( "file:///C:/Documents%20and%20Settings/davris/FileSchemeURIs.doc",
        "C:/Documents and Settings/davris/FileSchemeURIs.doc" );
      ("file:///abc/def/ghi.txt", "/abc/def/ghi.txt");
      ("file:///a:/bcd/efg/hij.txt", "a:/bcd/efg/hij.txt");
      ("file:///home/usr123/work/abc.txt", "/home/usr123/work/abc.txt");
      ("file:///usr/work/abc.txt", "/usr/work/abc.txt");
      ("file:///", "/");
      ("file://localhost/", "/");
      ("file:///c:", "/c:");
      ("file:///c|/path", "c:/path");
      ("file:///C:/Program%20Files", "C:/Program Files");
      ("file:///Macintosh%20HD/fileURLs/testof%3F.txt", "/Macintosh HD/fileURLs/testof?.txt");
      ("file:///fileURLs/test%3F.txt", "/fileURLs/test?.txt");
      ("file:///C:/WINDOWS/Desktop/FileURLs/has%23.txt", "C:/WINDOWS/Desktop/FileURLs/has#.txt");
      ("file:///Macintosh%20HD/fileURLs/testof%23.txt", "/Macintosh HD/fileURLs/testof#.txt");
      ("file:///fileURLs/testof%23.txt", "/fileURLs/testof#.txt");
      ("file:///C:/Program Files", "C:/Program Files");
      ("file:///C:/WINDOWS/Desktop/FileURLs/trailing.", "C:/WINDOWS/Desktop/FileURLs/trailing.");
      ("file:///Macintosh%20HD/fileURLs/trailing.", "/Macintosh HD/fileURLs/trailing.");
      ("file:///C:/Autoexec.bat", "C:/Autoexec.bat");
      ("file:///etc/hosts", "/etc/hosts");
      ( "file:///C:/WINDOWS/Desktop/FileURLs/mozilla-banner",
        "C:/WINDOWS/Desktop/FileURLs/mozilla-banner" );
      ("file:///Macintosh%20HD/fileURLs/mozilla-banner", "/Macintosh HD/fileURLs/mozilla-banner");
      ("file:///fileURLs/mozilla-banner", "/fileURLs/mozilla-banner");
      ("file:///wh/at!/ev%20/er", "/wh/at!/ev /er");
      ("file:///fi%6Ce", "/file");
      ("file:///fi%6ce", "/file");
    ]
  in
  let do_example (uri, expected) =
    let actual = File_url.parse uri in
    if actual <> expected then
      failwith (Printf.sprintf "Expected '%s' -> '%s', not '%s'" uri expected actual)
  in
  List.iter examples ~f:do_example;
  true

let test_invalid_parse () =
  let examples =
    [
      "file";
      "file:";
      "file:/";
      "file://";
      "file:path/path";
      "file:/path/path";
      "file:c|/path";
      "file:/C:/config.sys";
      "file://C:/config.sys";
      "file://alpha.hut.fi/u/lai/tik/tik76002/public_html/lerman.files/chaps";
      "file://server/share/My%20Documents%20100%2520/foo.txt";
      "file://laptop/My%20Documents/FileSchemeURIs.doc";
      "file:///C:/Program%20Files/Music/Web%20Sys/main.html?REQUEST=RADIO";
      "file://applib/products/a-b/abc_9/4148.920a/media/start.swf";
      "file://filename";
      "file://path1/path2/filename";
      "file://c|";
      "file://c:";
      "file://c|/";
      "file://c:/";
      "file:///Macintosh%20HD/fileURLs/fragment_test.html#here";
      "file:Macintosh%20HD/fileURLs/text.txt";
      "file:/Macintosh%20HD/fileURLs/text.txt";
      "file:fileURLs/text.txt";
      "file:/fileURLs/text.txt";
      "file://hostname/";
      "fool:///etc/hosts";
      "file://";
      "file://localhost/etc/%C3%B2.txt";
      "file:///c:/?";
      "file:///Macintosh%20HD/fileURLs/?";
      "file:///Macintosh%20HD/fileURLs/testof?.txt";
      "file:///fileURLs/?";
      "file:///fileURLs/testof?.txt";
      "file://///server/share/path";
      "file:////fileURLs/testof%23.txt";
      "file://user:pass@foo.com:123/wh/at/ever";
      "file://foo.com";
      "file://foo-bar.com";
      "file://local%68ost/path";
      "fi+le:";
      "fi+le:///path";
      "file:///fi%6ge";
      "file:///fi%6/stuff";
      "file:///fi%6";
      "file:///fi%ge";
      "file:///fi%/stuff";
      "file:///fi%";
    ]
  in
  let do_example uri =
    let did_raise =
      try
        let _ = File_url.parse uri in
        false
      with _ -> true
    in
    if not did_raise then failwith (Printf.sprintf "Expected '%s' not to parse" uri)
  in
  List.iter examples ~f:do_example;
  true

let test_create () =
  (* on Windows, `/` and `\` are both path separators that get encoded to `/`.
     on POSIX, `\` is a normal character and so it gets %-encoded to %5C. *)
  let examples =
    [
      ( "c:\\autoexec.bat",
        if Sys.win32 then
          "file:///c%3A/autoexec.bat"
        else
          "file:///c%3A%5Cautoexec.bat" );
      ("c:/autoexec.bat", "file:///c%3A/autoexec.bat");
      ( "c:\\\\autoexec.bat",
        if Sys.win32 then
          "file:///c%3A//autoexec.bat"
        else
          "file:///c%3A%5C%5Cautoexec.bat" );
      ("c://autoexec.bat", "file:///c%3A//autoexec.bat");
      ("/etc/dollar$dollar", "file:///etc/dollar$dollar");
      ("/etc/hash#hash", "file:///etc/hash%23hash");
      ("/etc/question?question", "file:///etc/question%3Fquestion");
      ( "/etc/braces{}/backtick`/caret^/space /file",
        "file:///etc/braces%7B%7D/backtick%60/caret%5E/space%20/file" );
    ]
  in
  let do_example (file, expected) =
    let actual = File_url.create file in
    if actual <> expected then
      failwith (Printf.sprintf "Expected '%s' -> '%s', not '%s'" file expected actual)
  in
  List.iter examples ~f:do_example;
  true

let tests =
  [
    ("test_valid_parse", test_valid);
    ("test_invalid_parse", test_invalid_parse);
    ("test_create", test_create);
  ]

let () = Unit_test.run_all tests
