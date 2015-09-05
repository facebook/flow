TOP=..

##############################################################################
# Variables
#############################################################################

SRC = \
  hh_json.ml\
  ide.ml\
  pos.ml\
  ident.ml\
  errors.ml\
  utils.ml\
  prefix.ml\
  value.ml\
  sharedMem.ml\
  autocomplete.ml\
  find_refs.ml\
  typing_deps.ml\
  namespace_env.ml\
  ast.ml\
  namespaces.ml\
  parser_heap.ml\
  lexing_modes.ml\
  lexer_hack.ml\
  parser_hack.ml\
  fileInfo.ml\
  typing_reason.ml\
  nast.ml\
  typing_defs.ml\
  typing_env.ml\
  naming_heap.ml\
  naming_ast_helpers.ml\
  naming.ml\
  prefix.ml\
  typing_print.ml\
  typing_utils.ml\
  typing_exts.ml\
  typing_instantiate.ml\
  nastVisitor.ml\
  typing_alias.ml\
  typing_tdef.ml\
  typing_unify.ml\
  typing_subtype.ml\
  typing_expand.ml\
  typing_generic.ml\
  typing_ops.ml\
  typing_dynamic_yield.ml\
  typing_enum.ml\
  typing_hint.ml\
  typing_suggest.ml\
  typing_async.ml\
  typing_extends.ml\
  nastCheck.ml\
  nast_terminality.ml\
  nastInitCheck.ml\
  typing_lenv.ml\
  typing.ml\
  typing_inherit.ml\
  typing_variance.ml\
  typing_decl.ml\
  pp.ml\
  colorFile.ml\
  fileOutline.ml\
  format_hack.ml\
  hh_ide.ml\
  typeVisitor.ml\
  coverage_level.ml\
  typingEqualityCheck.ml\
  typing_taccess.ml\

INCLUDEDIRS=$(shell ocamlfind -query js_of_ocaml)

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

all:: $(OBJS)
all.opt: $(OBJS)

# this takes 2s so it's disabled for now, and should be done only in deploy
hh_ide.js: $(OBJS)
	ocamlfind ocamlc -package js_of_ocaml -pp camlp4o -linkpkg $^ -o hh_ide.byte
	js_of_ocaml hh_ide.byte

deploy: hh_ide.js
	echo "copy hh_ide.js somewhere ..."

#TODO: a bid sad to do that, can probably just reuse the lib.cma
lexer_hack.ml: $(TOP)/parsing/lexer_hack.ml
	cp $(TOP)/parsing/lexer_hack.ml .

format_hack.ml: $(TOP)/parsing/format_hack.ml
	cp $(TOP)/parsing/format_hack.ml .

parser_hack.ml: $(TOP)/parsing/parser_hack.ml
	cp $(TOP)/parsing/parser_hack.ml .

pos.ml: $(TOP)/utils/pos.ml
	cp $(TOP)/utils/pos.ml .

ide.ml: $(TOP)/globals/ide.ml
	cp $(TOP)/globals/ide.ml .

prefix.ml: $(TOP)/heap/prefix.ml
	cp $(TOP)/heap/prefix.ml .

value.ml: $(TOP)/heap/value.ml
	cp $(TOP)/heap/value.ml .

socket.ml: $(TOP)/socket/socket.ml
	cp $(TOP)/socket/socket.ml .

namespace_env.ml: $(TOP)/parsing/namespace_env.ml
	cp $(TOP)/parsing/namespace_env.ml .

ast.ml: $(TOP)/parsing/ast.ml
	cp $(TOP)/parsing/ast.ml .

namespaces.ml: $(TOP)/parsing/namespaces.ml
	cp $(TOP)/parsing/namespaces.ml .

parser_heap.ml: $(TOP)/parsing/parser_heap.ml
	cp $(TOP)/parsing/parser_heap.ml .

parser.ml: $(TOP)/parsing/parser.ml
	cp $(TOP)/parsing/parser.ml .

lexer.ml: $(TOP)/parsing/lexer.ml
	cp $(TOP)/parsing/lexer.ml .

hackedLexer.ml: $(TOP)/parsing/hackedLexer.ml
	cp $(TOP)/parsing/hackedLexer.ml .

tokenType.ml: $(TOP)/parsing/tokenType.ml
	cp $(TOP)/parsing/tokenType.ml .

fileInfo.ml: $(TOP)/deps/fileInfo.ml
	cp $(TOP)/deps/fileInfo.ml .

lexing_modes.ml: $(TOP)/parsing/lexing_modes.ml
	cp $(TOP)/parsing/lexing_modes.ml .

typing_suggest.ml: $(TOP)/typing/typing_suggest.ml
	cp $(TOP)/typing/typing_suggest.ml .

typing_utils.ml: $(TOP)/typing/typing_utils.ml
	cp $(TOP)/typing/typing_utils.ml .

typeVisitor.ml: $(TOP)/typing/typeVisitor.ml
	cp $(TOP)/typing/typeVisitor.ml .

coverage_level.ml: $(TOP)/typing/coverage_level.ml
	cp $(TOP)/typing/coverage_level.ml .

typing_dynamic_yield.ml: $(TOP)/typing/typing_dynamic_yield.ml
	cp $(TOP)/typing/typing_dynamic_yield.ml .

typing_enum.ml: $(TOP)/typing/typing_enum.ml
	cp $(TOP)/typing/typing_enum.ml .

typing_print.ml: $(TOP)/typing/typing_print.ml
	cp $(TOP)/typing/typing_print.ml .

typing_exts.ml: $(TOP)/typing/typing_exts.ml
	cp $(TOP)/typing/typing_exts.ml .

typing_expand.ml: $(TOP)/typing/typing_expand.ml
	cp $(TOP)/typing/typing_expand.ml .

typing_async.ml: $(TOP)/typing/typing_async.ml
	cp $(TOP)/typing/typing_async.ml .

typing_hint.ml: $(TOP)/typing/typing_hint.ml
	cp $(TOP)/typing/typing_hint.ml .

typing_extends.ml: $(TOP)/typing/typing_extends.ml
	cp $(TOP)/typing/typing_extends.ml .

typing.ml: $(TOP)/typing/typing.ml
	cp $(TOP)/typing/typing.ml .

typing_variance.ml: $(TOP)/typing/typing_variance.ml
	cp $(TOP)/typing/typing_variance.ml .

typing_decl.ml: $(TOP)/typing/typing_decl.ml
	cp $(TOP)/typing/typing_decl.ml .

typing_inherit.ml: $(TOP)/typing/typing_inherit.ml
	cp $(TOP)/typing/typing_inherit.ml .

nast.ml: $(TOP)/naming/nast.ml
	cp $(TOP)/naming/nast.ml .

nastVisitor.ml: $(TOP)/naming/nastVisitor.ml
	cp $(TOP)/naming/nastVisitor.ml .

nastCheck.ml: $(TOP)/typing/nastCheck.ml
	cp $(TOP)/typing/nastCheck.ml .

nast_terminality.ml: $(TOP)/typing/nast_terminality.ml
	cp $^ .

nastInitCheck.ml: $(TOP)/typing/nastInitCheck.ml
	cp $(TOP)/typing/nastInitCheck.ml .

autocomplete.ml: $(TOP)/globals/autocomplete.ml
	cp $(TOP)/globals/autocomplete.ml .

find_refs.ml: $(TOP)/globals/find_refs.ml
	cp $(TOP)/globals/find_refs.ml .

naming_ast_helpers.ml: $(TOP)/naming/naming_ast_helpers.ml
	cp $^ .

naming_heap.ml: $(TOP)/naming/naming_heap.ml
	cp $(TOP)/naming/naming_heap.ml .

naming.ml: $(TOP)/naming/naming.ml
	cp $(TOP)/naming/naming.ml .

typing_reason.ml: $(TOP)/typing/typing_reason.ml
	cp $(TOP)/typing/typing_reason.ml .

typing_defs.ml: $(TOP)/typing/typing_defs.ml
	cp $(TOP)/typing/typing_defs.ml .

typing_env.ml: $(TOP)/typing/typing_env.ml
	cp $(TOP)/typing/typing_env.ml .

typing_ops.ml: $(TOP)/typing/typing_ops.ml
	cp $(TOP)/typing/typing_ops.ml .

fix_file.ml: $(TOP)/parsing/fix_file.ml
	cp $(TOP)/parsing/fix_file.ml .

colorFile.ml: $(TOP)/client/colorFile.ml
	cp $(TOP)/client/colorFile.ml .

typing_instantiate.ml: $(TOP)/typing/typing_instantiate.ml
	cp $(TOP)/typing/typing_instantiate.ml .

typing_alias.ml: $(TOP)/typing/typing_alias.ml
	cp $(TOP)/typing/typing_alias.ml .

typing_generic.ml: $(TOP)/typing/typing_generic.ml
	cp $(TOP)/typing/typing_generic.ml .

typing_unify.ml: $(TOP)/typing/typing_unify.ml
	cp $(TOP)/typing/typing_unify.ml .

typing_tdef.ml: $(TOP)/typing/typing_tdef.ml
	cp $(TOP)/typing/typing_tdef.ml .

typing_subtype.ml: $(TOP)/typing/typing_subtype.ml
	cp $(TOP)/typing/typing_subtype.ml .

typing_lenv.ml: $(TOP)/typing/typing_lenv.ml
	cp $(TOP)/typing/typing_lenv.ml .

fileOutline.ml: $(TOP)/server/fileOutline.ml
	cp $(TOP)/server/fileOutline.ml .


clean::
	rm -f *.byte hh_ide.js

clean::
	rm -f ide.ml
	rm -f pos.ml
	rm -f prefix.ml
	rm -f value.ml
	rm -f namespace_env.ml
	rm -f ast.ml
	rm -f namespaces.ml
	rm -f parser_heap.ml
	rm -f parser.ml
	rm -f lexer.ml
	rm -f hackedLexer.ml
	rm -f tokenType.ml
	rm -f lexing_modes.ml
	rm -f typing_suggest.ml
	rm -f typing.ml
	rm -f typing_decl.ml
	rm -f typing_inherit.ml
	rm -f typing_lenv.ml
	rm -f nast.ml
	rm -f nastVisitor.ml
	rm -f nastCheck.ml
	rm -f nastInitCheck.ml
	rm -f naming_heap.ml
	rm -f naming_ast_helpers.ml
	rm -f naming.ml
	rm -f typing_reason.ml
	rm -f typing_defs.ml
	rm -f typing_instantiate.ml
	rm -f typing_utils.ml
	rm -f typing_env.ml
	rm -f typing_ops.ml
	rm -f fix_file.ml
	rm -f typing_async.ml
	rm -f typing_expand.ml
	rm -f typing_alias.ml
	rm -f typing_tdef.ml
	rm -f typing_subtype.ml
	rm -f typing_generic.ml
	rm -f typing_extends.ml
	rm -f typing_exts.ml
	rm -f typing_hint.ml
	rm -f typing_print.ml
	rm -f typing_dynamic_yield.ml
	rm -f typing_enum.ml
	rm -f find_refs.ml
	rm -f autocomplete.ml
	rm -f nast_terminality.ml
	rm -f colorFile.ml
	rm -f fileInfo.ml
	rm -f lexer_hack.ml
	rm -f parser_hack.ml
	rm -f typing_unify.ml
	rm -f fileOutline.ml
	rm -f format_hack.ml

# legacy
clean::
	rm -f nast_helpers.ml

.depend:: $(SRC)
