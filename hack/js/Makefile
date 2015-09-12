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
  typing_heap.ml\
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
	cp $< .

format_hack.ml: $(TOP)/parsing/format_hack.ml
	cp $< .

parser_hack.ml: $(TOP)/parsing/parser_hack.ml
	cp $< .

pos.ml: $(TOP)/utils/pos.ml
	cp $< .

ide.ml: $(TOP)/globals/ide.ml
	cp $< .

prefix.ml: $(TOP)/heap/prefix.ml
	cp $< .

value.ml: $(TOP)/heap/value.ml
	cp $< .

socket.ml: $(TOP)/socket/socket.ml
	cp $< .

namespace_env.ml: $(TOP)/parsing/namespace_env.ml
	cp $< .

ast.ml: $(TOP)/parsing/ast.ml
	cp $< .

namespaces.ml: $(TOP)/parsing/namespaces.ml
	cp $< .

parser_heap.ml: $(TOP)/parsing/parser_heap.ml
	cp $< .

parser.ml: $(TOP)/parsing/parser.ml
	cp $< .

lexer.ml: $(TOP)/parsing/lexer.ml
	cp $< .

hackedLexer.ml: $(TOP)/parsing/hackedLexer.ml
	cp $< .

tokenType.ml: $(TOP)/parsing/tokenType.ml
	cp $< .

fileInfo.ml: $(TOP)/deps/fileInfo.ml
	cp $< .

lexing_modes.ml: $(TOP)/parsing/lexing_modes.ml
	cp $< .

typing_suggest.ml: $(TOP)/typing/typing_suggest.ml
	cp $< .

typing_utils.ml: $(TOP)/typing/typing_utils.ml
	cp $< .

typeVisitor.ml: $(TOP)/typing/typeVisitor.ml
	cp $< .

coverage_level.ml: $(TOP)/typing/coverage_level.ml
	cp $< .

typing_dynamic_yield.ml: $(TOP)/typing/typing_dynamic_yield.ml
	cp $< .

typing_enum.ml: $(TOP)/typing/typing_enum.ml
	cp $< .

typing_print.ml: $(TOP)/typing/typing_print.ml
	cp $< .

typing_exts.ml: $(TOP)/typing/typing_exts.ml
	cp $< .

typing_expand.ml: $(TOP)/typing/typing_expand.ml
	cp $< .

typing_async.ml: $(TOP)/typing/typing_async.ml
	cp $< .

typing_hint.ml: $(TOP)/typing/typing_hint.ml
	cp $< .

typing_extends.ml: $(TOP)/typing/typing_extends.ml
	cp $< .

typing.ml: $(TOP)/typing/typing.ml
	cp $< .

typing_variance.ml: $(TOP)/typing/typing_variance.ml
	cp $< .

typing_decl.ml: $(TOP)/typing/typing_decl.ml
	cp $< .

typing_inherit.ml: $(TOP)/typing/typing_inherit.ml
	cp $< .

nast.ml: $(TOP)/naming/nast.ml
	cp $< .

nastVisitor.ml: $(TOP)/naming/nastVisitor.ml
	cp $< .

nastCheck.ml: $(TOP)/typing/nastCheck.ml
	cp $< .

nast_terminality.ml: $(TOP)/typing/nast_terminality.ml
	cp $< .

nastInitCheck.ml: $(TOP)/typing/nastInitCheck.ml
	cp $< .

autocomplete.ml: $(TOP)/globals/autocomplete.ml
	cp $< .

find_refs.ml: $(TOP)/globals/find_refs.ml
	cp $< .

naming_ast_helpers.ml: $(TOP)/naming/naming_ast_helpers.ml
	cp $< .

naming_heap.ml: $(TOP)/naming/naming_heap.ml
	cp $< .

naming.ml: $(TOP)/naming/naming.ml
	cp $< .

typing_reason.ml: $(TOP)/typing/typing_reason.ml
	cp $< .

typing_defs.ml: $(TOP)/typing/typing_defs.ml
	cp $< .

typing_heap.ml: $(TOP)/typing/typing_heap.ml
	cp $< .

typing_env.ml: $(TOP)/typing/typing_env.ml
	cp $< .

typing_ops.ml: $(TOP)/typing/typing_ops.ml
	cp $< .

fix_file.ml: $(TOP)/parsing/fix_file.ml
	cp $< .

colorFile.ml: $(TOP)/client/colorFile.ml
	cp $< .

typing_instantiate.ml: $(TOP)/typing/typing_instantiate.ml
	cp $< .

typing_alias.ml: $(TOP)/typing/typing_alias.ml
	cp $< .

typing_generic.ml: $(TOP)/typing/typing_generic.ml
	cp $< .

typing_unify.ml: $(TOP)/typing/typing_unify.ml
	cp $< .

typing_tdef.ml: $(TOP)/typing/typing_tdef.ml
	cp $< .

typing_subtype.ml: $(TOP)/typing/typing_subtype.ml
	cp $< .

typing_lenv.ml: $(TOP)/typing/typing_lenv.ml
	cp $< .

fileOutline.ml: $(TOP)/server/fileOutline.ml
	cp $< .


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
	rm -f typing_heap.ml
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
