(define-library (schemacs editor)
  ;; This is the base package for the Schemacs editor, which is a clone
  ;; of Emacs written in Scheme. The reference implementation is
  ;; written in Guile Scheme which comes with an Emacs Lisp compiler,
  ;; and therefore is not just a clone of the Emacs editor, but of the
  ;; Emacs Lisp programming language.

  (import
    (scheme base)
    (scheme lazy)
    (scheme write)
    (only (scheme read) read)
    (only (scheme case-lambda) case-lambda)
    (only (srfi 28) format)
    (only (schemacs hash-table)
          make-hash-table  default-hash
          )
    (only (schemacs lens)
          view record-unit-lens lens-set update =>hash-key!)
    (only (schemacs editor command)
          command-type? command-procedure
          new-command show-command apply-command)
    (only (schemacs eval) eval-string)
    (prefix (schemacs keymap) km:)
    (prefix (schemacs editor-impl) *impl/)
    (only (schemacs editor-impl)
          make<cell-factory>
          factory-make-cell
          factory-set!cell-value
          )
    (only (schemacs pretty)
          pretty make<pp-state>
          print line-break bracketed qstr
          pp-state-output-port
          pp-state-line-buffer
          pp-state-indent-char
          pp-state-indent
          pp-line-indent-char
          pp-line-indent
          pp-line-string)
    )

  (export
   ;; A quick note on naming: names delimited with asterisks,
   ;; e.g. *default-keymap* are parameter objects. Asterisk-delimited
   ;; names beginning with "impl/..." for example
   ;; *IMPL/NEW-WINFRAME-VIEW* are parameters that SHOULD be
   ;; parameterized by the user interface backend, but are typically
   ;; parameterized with procedures that do nothing by default.

   ;; ---------------- Buffers ----------------
   buffer-type? buffer-cell
   =>buffer-handle =>buffer-view =>buffer-local-keymap
   *default-buffer-local-keymap*
   current-buffer
   selected-buffer

   ;; -------------- Mode Lines --------------
   ;; This includes the echo area
   line-display-type?
   new-line-display new-mode-line new-header-line new-echo-area
   =>line-display-view
   *mode-line-format*
   mode-line-display-items

   ;; ---------------- Windows ----------------
   window-type?
   window-cell window-buffer window-parent-frame
   =>window-buffer =>window-view =>window-local-keymap
   =>window-mode-line =>window-header-line
   *default-window-local-keymap*
   selected-window select-window

   ;; -------------- Minibuffers -------------
   new-minibuffer
   *default-keymap*

   ;; ---------------- Frames ----------------
   winframe-type? winframe-cell
   winframe-parent-editor
   =>winframe-window =>winframe-local-keymap =>winframe-view
   =>winframe-echo-area =>winframe-minibuffer =>winframe-prompt
   *default-winframe-local-keymap*
   *default-minibuffer-keymap*
   selected-frame
   key-event-handler
   format-message
   display-in-echo-area clear-echo-area
   simple-read-minibuffer read-from-minibuffer
   exit-minibuffer
   focus-minibuffer
   clear-minibuffer
   eval-minibuffe
   minibuffer-prompt-resume
   exit-minibuffer-with-return

   ;; ---------------- Editor ----------------
   editor-type? new-editor editor-cell
   =>editor-buffer-table
   =>editor-window-table
   =>editor-winframe-table
   =>editor-proc-table
   =>editor-base-keymap
   =>editor-minibuffer
   =>editor-view
   editor-messages
   current-editor

   ;; ---------------- Front-end API ----------------
   new-self-insert-keymap key-event-self-insert
   default-keymap
   print-exception-message
   self-insert-keymap self-insert-command
   delete-char delete-backward-char
   *unbound-key-index*  unbound-key-index-handler
   *waiting-key-index*  waiting-key-index-handler
   *dispatch-key-event* dispatch-key-event-handler
   insert get-buffer read-from-minibuffer
   eval-expression-string eval-expression
   print-to-buffer
   )

  ;; =======================
  (include "editor.scm")
  )
