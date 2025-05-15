(define-library (schemacs editor-impl)
  ;; This library provides a long list of parameters which must be
  ;; parameterized by a back-end implemention. APIs in lirbaries such
  ;; as `(SCHEMACS EDITOR)` call the procedures stored into these
  ;; parameters.

  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (schemacs elisp-eval environment)
          *elisp-input-port*
          *elisp-output-port*
          *elisp-error-port*
          )
    )

  (export
   cell-factory*
   new-buffer-view*
   delete-char*
   new-mode-line-view*
   new-echo-area-view*
   new-header-line-view*
   mode-line-display-items*
   new-window-view*
   new-minibuffer-view*
   new-winframe-view*
   display-in-echo-area*
   clear-echo-area*
   get-minibuffer-text*
   exit-minibuffer*
   focus-minibuffer*
   clear-minibuffer*
   new-editor-view*
   self-insert-command*
   insert-into-buffer*
   is-graphical-display?*
   is-buffer-modified?*
   current-editor-closure*
   selected-frame*
   current-buffer*
   select-window*
   command-error-default-function*
   prin1*
   princ*
   )

  (include "editor-impl.scm")
  )
