type modes = {
  mutable debug: bool;
  mutable all: bool;
  mutable weak: bool;
  mutable traces_enabled: bool;
  mutable newtraces_enabled: bool;
  mutable strict: bool;
  mutable console: bool;
  mutable json: bool;
  mutable quiet : bool;
}

let modes = {
  debug = false;
  all = false;
  weak = false;
  traces_enabled = false;
  newtraces_enabled = false;
  strict = false;
  console = false;
  json = false;
  quiet = true;
}

let debug_string f = if modes.debug then prerr_endline (f ()) else ()
