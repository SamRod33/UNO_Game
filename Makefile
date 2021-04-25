MODULES=card player state main computer facecards
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
CARDTEST=card_test.byte
PLAYERTEST=player_test.byte
STATETEST=state_test.byte
MAIN=main.byte

default: build

utop: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

card-test:
	$(OCAMLBUILD) -tag 'debug' $(CARDTEST) && ./$(CARDTEST) -runner sequential

player-test:
	$(OCAMLBUILD) -tag 'debug' $(PLAYERTEST) && ./$(PLAYERTEST) -runner sequential

state-test:
	$(OCAMLBUILD) -tag 'debug' $(STATETEST) && ./$(STATETEST) -runner sequential

uno:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _coverage bisect*.coverage _doc.public _doc.private uno-game.zip

zip:
	zip uno-game.zip *.ml* *.json *.sh INSTALL.txt _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
