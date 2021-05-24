MODULES=card player state main computer constants window_gui MainFunctions
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
CARDTEST=card_test.byte
PLAYERTEST=player_test.byte
STATETEST=state_test.byte
COMPUTERTEST=computer_test.byte
TESTS=full_test_suite.byte
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

computer-test:
	$(OCAMLBUILD) -tag 'debug' $(COMPUTERTEST) && ./$(COMPUTERTEST) -runner sequential

tests:
	$(OCAMLBUILD) -tag 'debug' $(TESTS) && ./$(TESTS) -runner sequential


uno:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TESTS) && ./$(TESTS) -runner sequential

bisect: clean bisect-test
	bisect-ppx-report html


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

window:
	ocamlbuild -package graphics window_main.native -use-ocamlfind
	./window_main.native

ch_c_win:
	ocamlbuild -package graphics window_change_color.native -use-ocamlfind
	./window_change_color.native

window_swap_player:
	ocamlbuild -package graphics window_swap_player.native -use-ocamlfind
	./window_swap_player.native

intermission:
	ocamlbuild -package graphics window_intermission.native -use-ocamlfind
	./window_intermission.native

tise:
	ocamlbuild -package graphics window_tise_main.native -use-ocamlfind
	./window_tise_main.native

