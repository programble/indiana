#!/bin/sh
set -eu

case `uname` in
  Darwin)
    nasm -P darwin.asm -f macho64 indy.asm
    ld -o indy indy.o
    ;;
  *)
    echo "unsupported: `uname`"
    exit 1
esac
