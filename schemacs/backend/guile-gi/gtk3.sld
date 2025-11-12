(define-library (schemacs backend guile-gi gtk3)
  (import
    (scheme base)
    (only (schemacs eval) eval interaction-environment)
    (only (schemacs backend guile-gi gtk3-init) launch-gui))

  (export main)

  (begin

    (define init-expr
      '(begin
         (import
           (scheme base)
           (scheme case-lambda)
           (scheme lazy)
           (scheme char)
           (scheme complex)
           (scheme inexact)
           (scheme cxr)
           (only (schemacs eval)
                 eval null-environment
                 scheme-report-environment
                 interaction-environment)
           (schemacs editor)
           (schemacs keymap)
           (schemacs pretty)
           (schemacs lens)
           (only (schemacs backend guile-gi gtk3-init) launch-gui))
        ;; return the function used to launch the GUI
        launch-gui
        ))

    (define (main start-repl?)
      ;; This procedure will run `EVAL` to setup the initial
      ;; `INTERACTION-ENVIRONMENT`, then initialize the text editor state
      ;; inside of this initial environment. If the `START-REPL?` argument
      ;; is not #f, a child thread running a REPL will also start running
      ;; and read commands from `CURRENT-INPUT-PORT` to be evaluated in
      ;; the Gtk main event loop, the idea being to allow the controlling
      ;; terminal to evaluate expressions that modify the running Gtk
      ;; application.
      ;;------------------------------------------------------------------
      (let*((env (interaction-environment))
            (launch-gui (eval init-expr env))
            )
        (launch-gui start-repl? env)
        ))

    ;;----------------------------------------------------------------
    ))
