(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class ['loc] lexical_hoister :
  flowmin_compatibility:bool
  -> enable_enums:bool
  -> object
       inherit ['loc Bindings.t, 'loc] Flow_ast_visitor.visitor

       method nonlexical_statement : ('loc, 'loc) Ast.Statement.t -> ('loc, 'loc) Ast.Statement.t

       method flowmin_compatibility_statement :
         ('loc, 'loc) Ast.Statement.t -> ('loc, 'loc) Ast.Statement.t

       method base_statement : ('loc, 'loc) Ast.Statement.t -> ('loc, 'loc) Ast.Statement.t

       method base_pattern :
         ?kind:Ast.Variable.kind -> ('loc, 'loc) Ast.Pattern.t -> ('loc, 'loc) Ast.Pattern.t
     end

class ['loc] hoister :
  flowmin_compatibility:bool
  -> enable_enums:bool
  -> with_types:bool
  -> object
       inherit ['loc] lexical_hoister
     end
