
SCHEME_LIBRARIES := \
  ./chibi/match.sld \
  ./slib/common.sld \
  ./slib/filename.sld \
  ./slib/directory.sld \
  ./schemacs/bitwise.sld \
  ./schemacs/string.sld \
  ./schemacs/vector.sld \
  ./schemacs/hash-table.sld \
  ./schemacs/lens.sld \
  ./schemacs/cursor.sld \
  ./schemacs/lens/vector.sld \
  ./schemacs/pretty.sld \
  ./schemacs/lens/bin-hash-table.sld \
  ./schemacs/lexer.sld \
  ./schemacs/editor/command.sld \
  ./schemacs/keymap.sld \
  ./schemacs/bit-stack.sld \
  ./schemacs/elisp-eval/pretty.sld \
  ./schemacs/elisp-eval/parser.sld \
  ./schemacs/elisp-eval/environment.sld \
  ./schemacs/elisp-eval/format.sld \
  ./schemacs/editor-impl.sld \
  ./schemacs/elisp-eval.sld \
  ./schemacs/elisp-load.sld \


SEARCH_PATHS := . \
  ./chibi/ \
  ./slib/ \
  ./schemacs/lens/ \
  ./schemacs/editor/ \
  ./schemacs/elisp-eval/ \
  ./schemacs/ \


######################################################################

.PHONY: clean  clean-gambit  cleam-mit  schemacs-guile  schemacs-gambit

schemacs-mitscheme.com: build.scm $(SCHEME_LIBRARIES)
	mit-scheme --eval '(let () (load "build.scm") (exit 0))';

schemacs-guile: $(SCHEME_LIBRARIES)
	guile -c '(let () (load "./build.scm") (exit 0))';


schemacs-gambit: $(SCHEME_LIBRARIES)
	gsc -:r7rs . $(SEARCH_PATHS) $(SCHEME_LIBRARIES)

schemacs-stklos: $(SCHEME_LIBRARIES)
	stklos -l "./build.scm";

######################################################################

FIND_SKIP_GIT = find . -type d \( -name .git -o -name .akku \) -false -o

clean: clean-mit clean-stklos clean-gambit


clean-stklos:
	find -type f -name '*.ostk' -print -delete;


clean-mit:
	$(FIND_SKIP_GIT) \
	  -type f \
	  \( -name '*.com' \
	  -o -name '*.bin' \
	  -o -name '*.bci' \
	  -o -name '*.comld' \
	  -o -name '*.binld' \
	  -o -name '*.bcild' \
	  \) \
		-print -delete;


clean-gambit:
	$(FIND_SKIP_GIT) \
	  -type f \
	  \( -name '*.o[0-9]' \
	  -o -name '*.o[0-9][0-9]' \
	  \) \
	  -print -delete;
