#! /usr/bin/env sh
exec find . \
  -type d \
    \( -name .git \
    -o -name .akku \
    -o -name .build \
    \) \
  -prune -false \
  -o "${@}";
