{
  "targets": [
    {
      "target_name": "flow_parser",
      "dependencies": ["build_deps"],
      "sources": [
        "src/flow_parser_node.cc",
      ],
      "include_dirs": [
        "include/",
        "<!(node -e \"require('nan')\")",
      ],
      "libraries": [
        "-L../lib/<(OS)",
        "-lflowparser",
      ],
      "conditions": [
        ['OS=="mac"', {
          "xcode_settings": {
            "OTHER_LDFLAGS": [
              # suppress this warning:
              #   ld: warning: could not create compact unwind for
              #   _caml_start_program: dwarf uses DW_CFA_same_value
              "-Wl,-no_compact_unwind",
            ],
          },
        }],
      ],
    },
    {
      "target_name": "build_deps",
      "type": "none",
      "actions": [
        {
          "action_name": "make_deps",
          "inputs": [],
          "outputs": ["lib/<(OS)/libflowparser.a"],
          "conditions": [
            ['OS=="mac"', {"action": ["make", "deps-mac"]}],
            ['OS=="win"', {"action": ["make", "deps-win"]}],
            ['OS=="linux"', {"action": ["make", "deps-linux"]}],
          ],
        }
      ]
    }
  ]
}
