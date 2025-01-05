# Nom de l'exécutable final
EXEC = steiner

# Fichiers sources
SOURCES = input.ml output.ml graph.mli graph.ml
OBJECTS = $(SOURCES:.ml=.cmo)

# Compilateur OCaml
OCAMLC = ocamlc
GRAPHICS_PATH = ~/.opam/default/lib/graphics
OCAMLFLAGS = -I $(GRAPHICS_PATH) graphics.cma

# Règle par défaut : compilation complète
all: $(EXEC)

# Compilation de l'exécutable
$(EXEC): $(OBJECTS)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# Compilation des fichiers .ml en .cmo
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Compilation des fichiers .mli en .cmi
%.cmi: %.mli
	$(OCAMLC) -c $<

# Dépendance explicite :
graph.cmo: graph.cmi

# Nettoyage des fichiers générés
clean:
	rm -f *.cmo *.cmi $(EXEC)

.PHONY: all clean
