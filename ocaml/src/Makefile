# src/Makefile

MODULES = \
  tokens \
  ast \
  lexer \
  parser \
  codegen \
  main

%.cmo: %.ml
	ocamlc -c $<

compile:
	./toyc input.toy
	nasm -f elf64 input.asm -o output.o
	gcc -o run output.o 

all: toyc

toyc: $(MODULES:=.cmo)
	ocamlc -o $@ $^

clean:
	rm -f *.cmi *.cmo toyc
