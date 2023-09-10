{
  "variables": {
    "conditions": [
      ['OS=="mac"', {"platform": "osx"}],
      ['OS=="win"', {"platform": "win64"}],
      ['OS=="linux"', {"platform": "linux64"}],
    ],
  },
  "targets": [
    {
      "target_name": "flow_parser",
      "sources": [
        "src/flow_parser_node.cc",
      ],
      "include_dirs": [
        "<!(opam var lib)/ocaml",
        "include/",
        "node_modules/node-addon-api",
        "<!(node -e \"require('node-addon-api').include\")",
      ],
      "libraries": [
        "-L<!(opam var lib)/ocaml",
        "-L../lib/<(platform)",
        "-lflowparser",
        "-lunix"
      ],
      'defines': [ 'NAPI_DISABLE_CPP_EXCEPTIONS' ],
      "conditions": [
        ['OS=="mac"', {
          "xcode_settings": {
            "OTHER_LDFLAGS": [
              # suppress this warning:
              #   ld: warning: could not create compact unwind for
              #   _caml_start_program: dwarf uses DW_CFA_same_value
              "-Wl,-no_compact_unwind",
            ],
            # travis builds libflowparser on 10.12. if this is a problem,
            # we need to set MACOSX_DEPLOYMENT_TARGET in Travis when
            # compiling ocaml and libflowparser.
            "MACOSX_DEPLOYMENT_TARGET": "10.12",
          },
        }],
      ],
    }
  ]
}
