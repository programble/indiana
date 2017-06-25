#!/bin/sh
set -eu

build() {
  nasm -P $1 -f $2 indy.asm
  ld -o indy indy.o
}

case `uname` in
  Darwin)  build darwin.asm macho64 ;;
  FreeBSD) build freebsd.asm elf64 ;;
  Linux)   build linux.asm elf64 ;;
  *)
    echo "unsupported: `uname`"
    exit 1
esac
