#!/bin/sh

ocamlc -c bit_reader.ml
ocamlc -c state.ml
ocamlc -c traverse.ml
ocamlc -c squish.ml
ocamlc -c command_line.ml
ocamlc -o squish bit_reader.cmo state.cmo traverse.cmo squish.cmo command_line.cmo
