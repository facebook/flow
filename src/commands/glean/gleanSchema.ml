(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_json

let key v = JSON_Object [("key", v)]

module Src = struct
  module File = struct
    type t = string

    let to_json str = key (JSON_String str)
  end

  module FileLines = struct
    type t = {
      file: File.t;
      lengths: int list;
      endsInNewline: bool;
      hasUnicodeOrTabs: bool;
    }

    let to_json { file; lengths; endsInNewline; hasUnicodeOrTabs } =
      key
        (JSON_Object
           [
             ("file", File.to_json file);
             ( "lengths",
               JSON_Array (Base.List.map ~f:(fun n -> JSON_Number (string_of_int n)) lengths)
             );
             ("endsInNewline", JSON_Bool endsInNewline);
             ("hasUnicodeOrTabs", JSON_Bool hasUnicodeOrTabs);
           ]
        )
  end

  module ByteSpan = struct
    type t = {
      start: int;
      length: int;
    }

    let to_json { start; length } =
      JSON_Object
        [
          ("start", JSON_Number (string_of_int start));
          ("length", JSON_Number (string_of_int length));
        ]
  end
end

module Module = struct
  type t =
    | File of Src.File.t
    | Builtin
    | Lib of string
    | NoSource
    | String of string

  let of_file_key ~root ~write_root file_key =
    match Reason.string_of_source ~strip_root:(Some root) file_key with
    | "(builtin)" -> Builtin
    | str when String.length str >= 5 && String.sub str 0 5 = "[LIB]" ->
      Lib (Base.String.subo ~pos:6 str)
    | str -> File (Filename.concat write_root str)

  let of_loc_source ~root ~write_root = function
    | None -> NoSource
    | Some file_key -> of_file_key ~root ~write_root file_key

  let of_modulename ~root ~write_root = function
    | Modulename.String str -> String str
    | Modulename.Filename file_key -> of_file_key ~root ~write_root file_key

  let to_json module_ =
    key
    @@ JSON_Object
         [
           (match module_ with
           | File file -> ("file", Src.File.to_json file)
           | Builtin -> ("builtin", JSON_Object [])
           | Lib lib -> ("lib", JSON_String lib)
           | NoSource -> ("noSource", JSON_Object [])
           | String str -> ("string_", JSON_String str));
         ]
end

module FileOfStringModule = struct
  type t = {
    file: Src.File.t;
    string: string;
  }

  let to_json { file; string } =
    key @@ JSON_Object [("file", Src.File.to_json file); ("string_", JSON_String string)]
end

module Range = struct
  type t = Loc.t

  let to_json ~root ~write_root Loc.{ source; start; _end } =
    let span =
      match Base.Option.bind ~f:Offset_cache.offset_table_of_file_key source with
      | None -> Src.ByteSpan.{ start = 0; length = 0 }
      | Some table ->
        let start = Offset_utils.offset table start in
        let length = Offset_utils.offset table _end - start in

        Src.ByteSpan.{ start; length }
    in

    key
    @@ JSON_Object
         [
           ("module", Module.(source |> of_loc_source ~root ~write_root |> to_json));
           ("span", Src.ByteSpan.to_json span);
         ]
end

module Name = struct
  type t = string

  let to_json name = key @@ JSON_String name
end

module Type = struct
  type t = string

  let to_json name = key @@ JSON_String name
end

module Documentation = struct
  type t = Range.t

  let to_json ~root ~write_root loc = key @@ Range.to_json ~root ~write_root loc
end

module Declaration = struct
  type t = {
    name: Name.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { name; loc } =
    key @@ JSON_Object [("name", Name.to_json name); ("loc", Range.to_json ~root ~write_root loc)]
end

module DeclarationInfo = struct
  type t = {
    declaration: Declaration.t;
    type_: Type.t;
    documentation: Documentation.t option;
  }

  let to_json ~root ~write_root { declaration; type_; documentation } =
    key
    @@ JSON_Object
         (("declaration", Declaration.to_json ~root ~write_root declaration)
          ::
          ("type", Type.to_json type_)
          ::
          (match documentation with
          | None -> []
          | Some loc -> [("documentation", Documentation.to_json ~root ~write_root loc)])
         )
end

module LocalDeclarationReference = struct
  type t = {
    declaration: Declaration.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { declaration; loc } =
    key
    @@ JSON_Object
         [
           ("declaration", Declaration.to_json ~root ~write_root declaration);
           ("loc", Range.to_json ~root ~write_root loc);
         ]
end

module MemberDeclaration = struct
  type t = {
    name: Name.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { name; loc } =
    key @@ JSON_Object [("name", Name.to_json name); ("loc", Range.to_json ~root ~write_root loc)]
end

module MemberDeclarationReference = struct
  type t = {
    memberDeclaration: MemberDeclaration.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { memberDeclaration; loc } =
    key
    @@ JSON_Object
         [
           ("memberDeclaration", MemberDeclaration.to_json ~root ~write_root memberDeclaration);
           ("loc", Range.to_json ~root ~write_root loc);
         ]
end

module MemberDeclarationInfo = struct
  type t = {
    memberDeclaration: MemberDeclaration.t;
    type_: Type.t;
    documentation: Documentation.t option;
  }

  let to_json ~root ~write_root { memberDeclaration; type_; documentation } =
    key
    @@ JSON_Object
         (("memberDeclaration", MemberDeclaration.to_json ~root ~write_root memberDeclaration)
          ::
          ("type", Type.to_json type_)
          ::
          (match documentation with
          | None -> []
          | Some loc -> [("documentation", Documentation.to_json ~root ~write_root loc)])
         )
end

module Export = struct
  type t =
    | CommonJS
    | CommonJSMember of Name.t
    | Named of Name.t
    | Default
    | Star of Module.t

  let to_json export =
    key
    @@ JSON_Object
         [
           (match export with
           | CommonJS -> ("commonJS", JSON_Object [])
           | CommonJSMember name -> ("commonJSMember", Name.to_json name)
           | Named name -> ("named", Name.to_json name)
           | Default -> ("default_", JSON_Object [])
           | Star module_ -> ("star", Module.to_json module_));
         ]
end

module ModuleExport = struct
  type t = {
    module_: Module.t;
    export: Export.t;
  }

  let to_json { module_; export } =
    key @@ JSON_Object [("module", Module.to_json module_); ("export_", Export.to_json export)]
end

module ImportDeclaration = struct
  type import =
    | ModuleExport of ModuleExport.t
    | ModuleNamespace of Module.t

  type t = {
    declaration: Declaration.t;
    import: import;
  }

  let import_to_json import =
    JSON_Object
      [
        (match import with
        | ModuleExport moduleExport -> ("moduleExport", ModuleExport.to_json moduleExport)
        | ModuleNamespace module_ -> ("moduleNamespace", Module.to_json module_));
      ]

  let to_json ~write_root ~root { declaration; import } =
    key
    @@ JSON_Object
         [
           ("declaration", Declaration.to_json ~write_root ~root declaration);
           ("import_", import_to_json import);
         ]
end

module SourceOfExport = struct
  type source =
    | Declaration of Declaration.t
    | MemberDeclaration of MemberDeclaration.t
    | ModuleExport of ModuleExport.t
    | ModuleNamespace of Module.t

  type t = {
    moduleExport: ModuleExport.t;
    source: source;
  }

  let source_to_json ~root ~write_root source =
    JSON_Object
      [
        (match source with
        | Declaration declaration ->
          ("declaration", Declaration.to_json ~root ~write_root declaration)
        | MemberDeclaration memberDeclaration ->
          ("memberDeclaration", MemberDeclaration.to_json ~root ~write_root memberDeclaration)
        | ModuleExport moduleExport -> ("moduleExport", ModuleExport.to_json moduleExport)
        | ModuleNamespace module_ -> ("moduleNamespace", Module.to_json module_));
      ]

  let to_json ~root ~write_root { moduleExport; source } =
    key
    @@ JSON_Object
         [
           ("moduleExport", ModuleExport.to_json moduleExport);
           ("source", source_to_json ~root ~write_root source);
         ]
end

module TypeDeclaration = struct
  type t = {
    name: Name.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { name; loc } =
    key @@ JSON_Object [("name", Name.to_json name); ("loc", Range.to_json ~root ~write_root loc)]
end

module TypeDeclarationReference = struct
  type t = {
    typeDeclaration: TypeDeclaration.t;
    loc: Range.t;
  }

  let to_json ~root ~write_root { typeDeclaration; loc } =
    key
    @@ JSON_Object
         [
           ("typeDeclaration", TypeDeclaration.to_json ~root ~write_root typeDeclaration);
           ("loc", Range.to_json ~root ~write_root loc);
         ]
end

module TypeDeclarationInfo = struct
  type t = {
    typeDeclaration: TypeDeclaration.t;
    type_: Type.t;
    documentation: Documentation.t option;
  }

  let to_json ~root ~write_root { typeDeclaration; type_; documentation } =
    key
    @@ JSON_Object
         (("typeDeclaration", TypeDeclaration.to_json ~root ~write_root typeDeclaration)
          ::
          ("type", Type.to_json type_)
          ::
          (match documentation with
          | None -> []
          | Some loc -> [("documentation", Documentation.to_json ~root ~write_root loc)])
         )
end

module TypeExport = struct
  type t =
    | Named of Name.t
    | Star of Module.t

  let to_json type_export =
    key
    @@ JSON_Object
         [
           (match type_export with
           | Named name -> ("named", Name.to_json name)
           | Star module_ -> ("star", Module.to_json module_));
         ]
end

module ModuleTypeExport = struct
  type t = {
    module_: Module.t;
    typeExport: TypeExport.t;
  }

  let to_json { module_; typeExport } =
    key
    @@ JSON_Object
         [("module", Module.to_json module_); ("typeExport", TypeExport.to_json typeExport)]
end

module TypeImportDeclaration = struct
  type import =
    | Type of ModuleTypeExport.t
    | Typeof of ModuleExport.t
    | ModuleTypeof of Module.t

  type t = {
    typeDeclaration: TypeDeclaration.t;
    import: import;
  }

  let import_to_json import =
    JSON_Object
      [
        (match import with
        | Type type_ -> ("type", ModuleTypeExport.to_json type_)
        | Typeof typeof -> ("typeof_", ModuleExport.to_json typeof)
        | ModuleTypeof module_ -> ("moduleTypeof", Module.to_json module_));
      ]

  let to_json ~root ~write_root { typeDeclaration; import } =
    key
    @@ JSON_Object
         [
           ("typeDeclaration", TypeDeclaration.to_json ~root ~write_root typeDeclaration);
           ("import_", import_to_json import);
         ]
end

module SourceOfTypeExport = struct
  type source =
    | TypeDeclaration of TypeDeclaration.t
    | ModuleTypeExport of ModuleTypeExport.t
    | ModuleNamespace of Module.t

  type t = {
    moduleTypeExport: ModuleTypeExport.t;
    source: source;
  }

  let source_to_json ~root ~write_root source =
    JSON_Object
      [
        (match source with
        | TypeDeclaration typeDeclaration ->
          ("typeDeclaration", TypeDeclaration.to_json ~root ~write_root typeDeclaration)
        | ModuleTypeExport moduleTypeExport ->
          ("moduleTypeExport", ModuleTypeExport.to_json moduleTypeExport)
        | ModuleNamespace module_ -> ("moduleNamespace", Module.to_json module_));
      ]

  let to_json ~root ~write_root { moduleTypeExport; source } =
    key
    @@ JSON_Object
         [
           ("moduleTypeExport", ModuleTypeExport.to_json moduleTypeExport);
           ("source", source_to_json ~root ~write_root source);
         ]
end
