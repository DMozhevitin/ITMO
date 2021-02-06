SOURCES=Main.hs Lex.hs Parser.hs
  
.PHONY: all run 

all: parser

run:
	./parser

parser: $(SOURCES)
	ghc  -O2 Main.hs -o parser