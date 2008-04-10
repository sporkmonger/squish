#!/bin/sh

compile.sh
rlwrap ocaml bit_reader.cmo state.cmo traverse.cmo
