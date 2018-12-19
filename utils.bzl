load("@fbcode_macros//build_defs:platform_utils.bzl", "platform_utils")

def get_ppx_bin():
    return "third-party-buck/{}/build/supercaml/share/dotopam/default/lib/lwt_ppx/ppx.exe".format(platform_utils.get_platform_for_base_path(get_base_path()))
