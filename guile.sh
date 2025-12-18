#!/usr/bin/env sh

# Checks if the $GUILE_REPL_IN_GDB variable has been exported as
# 'true' or 1, if so runs the Guile interpreter in GDB. This is so
# that segfaults caused by imported library code or by FFI code, can
# be caught and inspected.

A='guile --r7rs';
#A="${A} --fresh-auto-compile"
A="${A} -L ${PWD}";
GUILE_CMD_ARGS="${A}";
unset A;

GUILE_REPL_IN_GDB=false;
GUILE_IN_GUIX_SHELL=false;

#export GDK_BACKEND='x11,wayland';
export GTK_DEBUG='interactive';
#export GOBJECT_DEBUG='instance-count';

#---------------------------------------------------------------------

if ${GUILE_IN_GUIX_SHELL}; then
   GUIX_SHELL_CMD='guix shell --pure -p ./.guix-profile --';
   echo 'Using guix shell to run Guile';
   echo "GUIX_SHELL_CMD=\"${GUIX_SHELL_CMD}\"";
else
   GUIX_SHELL_CMD='';
fi;

type guile;
echo "Guile CLI arguments: ${GUILE_CMD_ARGS} ${@}";

if ${GUILE_REPL_IN_GDB}; then
  exec ${GUIX_SHELL_CMD} \
    gdb \
      -ex 'handle SIGXCPU nostop' \
      -ex 'handle SIGPWR nostop' \
      -ex 'run' \
      --args \
        ${GUILE_CMD_ARGS} "${@}";
else
  exec ${GUIX_SHELL_CMD} ${GUILE_CMD_ARGS} "${@}";
fi;
