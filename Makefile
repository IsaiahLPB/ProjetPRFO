# Nom de l'exécutable final
EXEC = steiner

# Liste des fichiers source
MLFILES = graph.ml
MLIFILES = graph.mli

# Chemin de la bibliothèque Graphics
GRAPHICS_PATH = ~/.opam/default/lib/graphics

# Commande OCaml
OCAMLC = ocamlc
OCAMLFLAGS = -I $(GRAPHICS_PATH) graphics.cma

# Fichiers intermédiaires
CMO = $(MLFILES:.ml=.cmo)
CMI = $(MLIFILES:.mli=.cmi)

# Compilation finale
all: $(EXEC)

# Compilation des fichiers .mli en .cmi
%.cmi: %.mli
	$(OCAMLC) -c $<

# Compilation des fichiers .ml en .cmo
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Construction de l'exécutable
$(EXEC): $(CMO)
	$(OCAMLC) $(OCAMLFLAGS) $^ -o $@

# Nettoyage des fichiers générés
clean:
	rm -f *.cmo *.cmi *.o

# Suppression de tous les fichiers générés, y compris l'exécutable
distclean: clean
	rm -f $(EXEC)
