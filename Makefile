# Nom de l'exécutable final
EXEC = steiner

# Liste des fichiers source
MLFILES = graph.ml
MLIFILES = graph.mli

# Options de compilation
OCAMLC = ocamlc
OCAMLFLAGS = -g
OCAMLBUILD = $(OCAMLC) $(OCAMLFLAGS)

# Fichiers intermédiaires
CMO = $(MLFILES:.ml=.cmo)
CMI = $(MLIFILES:.mli=.cmi)

# Cibles principales
all: $(EXEC)

# Compilation des fichiers .mli en .cmi
%.cmi: %.mli
	$(OCAMLC) -c $<

# Compilation des fichiers .ml en .cmo
%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

# Construction de l'exécutable
$(EXEC): $(CMO)
	$(OCAMLC) -o $(EXEC) $(CMO)

# Nettoyage des fichiers générés
clean:
	rm -f *.cmo *.cmi *.o *.annot

# Suppression de tous les fichiers générés, y compris l'exécutable
distclean: clean
	rm -f $(EXEC)
