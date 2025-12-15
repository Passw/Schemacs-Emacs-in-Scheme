(cond-expand

  (guile-3

   (load "./main-gui.scm")

   )

  (gambit
   (begin

     (load "./chibi/match.sld")
     (load "./slib/common.sld")
     (load "./slib/filename.sld")
     (load "./slib/directory.sld")
     (load "./schemacs/bitwise.sld")
     (load "./schemacs/string.sld")
     (load "./schemacs/vector.sld")
     (load "./schemacs/hash-table.sld")
     (load "./schemacs/lens.sld")
     (load "./schemacs/cursor.sld")
     (load "./schemacs/lens/vector.sld")
     (load "./schemacs/pretty.sld")
     (load "./schemacs/lens/bin-hash-table.sld")
     (load "./schemacs/lexer.sld")
     (load "./schemacs/editor/command.sld")
     (load "./schemacs/keymap.sld")
     (load "./schemacs/bit-stack.sld")
     (load "./schemacs/elisp-eval/pretty.sld")
     (load "./schemacs/elisp-eval/parser.sld")
     (load "./schemacs/elisp-eval/environment.sld")
     (load "./schemacs/elisp-eval/format.sld")
     (load "./schemacs/editor-impl.sld")
     (load "./schemacs/elisp-eval.sld")
     (load "./schemacs/elisp-load.sld")

     ))

  (else

   (display "Sorry, this scheme implementation does not appear to be supported.")
   (newline)

   ))
