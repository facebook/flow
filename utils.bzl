load("@fbcode_macros//build_defs:platform.bzl", "platform")

def get_ppx_bin():
    return 'third-party-buck/{}/build/ocaml-lwt_ppx/lib/lwt_ppx/ppx.exe'.format(platform.get_platform_for_base_path(get_base_path()))
