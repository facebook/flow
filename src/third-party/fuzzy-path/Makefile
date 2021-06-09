
ifeq ($(OS), Windows_NT)
  CXX:=x86_64-w64-mingw32-g++
  AR:=x86_64-w64-mingw32-gcc-ar
endif

OCAML_INCLUDES := $(shell ocamlc 2>/dev/null -where || echo /usr/local/lib/ocaml)

CFLAGS= -I $(OCAML_INCLUDES)

CXXFLAGS=-std=c++11 -Wall -O3 

libfuzzy-path.a: src/fuzzy_path_stubs.o src/fuzzy_path_wrapper.o vendor/MatcherBase.o vendor/score_match.o
	$(AR) $(ARFLAGS) $@ $^
clean:
	rm  -rf *.o *.a src/*.o vendor/*.o

# Those deps are listed manually since they rarely changed
vendor/MatcherBase.o : vendor/MatcherBase.cpp vendor/MatcherBase.h vendor/score_match.h	
vendor/score_match.o : vendor/score_match.cpp vendor/score_match.h
src/fuzzy_path_wrapper.o : src/fuzzy_path_wrapper.cpp src/fuzzy_path_wrapper.h vendor/MatcherBase.h
src/fuzzy_path_stubs.o : src/fuzzy_path_stubs.c src/fuzzy_path_wrapper.h