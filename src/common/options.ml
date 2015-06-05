type options = {
  opt_debug : bool;
  opt_verbose : bool;
  opt_all : bool;
  opt_weak : bool;
  opt_traces : int;
  opt_strict : bool;
  opt_console : bool;
  opt_json : bool;
  opt_show_all_errors : bool;
  opt_quiet : bool;
  opt_profile : bool;
  opt_strip_root : bool;
  opt_module: string;
  opt_libs: Path.t list;
  opt_no_flowlib: bool;
  opt_module_name_mappers: (Str.regexp * string) list;
}

let default_options = {
  opt_debug = false;
  opt_verbose = false;
  opt_all = false;
  opt_weak = false;
  opt_traces = 0;
  opt_strict = false;
  opt_console = false;
  opt_json = false;
  opt_show_all_errors = false;
  opt_quiet = false;
  opt_profile = false;
  opt_strip_root = false;
  opt_module = FlowConfig.(
    match default_module_system with
    | Node -> "node"
    | Haste -> "haste"
  );
  opt_libs = [];
  opt_no_flowlib = false;
  opt_module_name_mappers = [];
}
