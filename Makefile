MODULES=card player state main computer constants testHelper test windowGui mainFunctions windowChangeColor windowIntermission windowSwapPlayer windowMain windowIntro winSelect windowHelp windowEndGame
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
CARDTEST=cardTest.byte
PLAYERTEST=playerTest.byte
STATETEST=stateTest.byte
COMPUTERTEST=computerTest.byte
TEST=test.byte
MAIN=main.byte
GUI=GUIMain.byte

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

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential


uno:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

uno-gui:
	$(OCAMLBUILD) -tag 'debug' $(GUI) && OCAMLRUNPARAM=b ./$(GUI)

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

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
	ocamlbuild -package graphics windowMain.native -use-ocamlfind
	./WindowMain.native

change-color:
	ocamlbuild -package graphics windowChangeColor.native -use-ocamlfind
	./WindowChangeColor.native

window-swap-player:
	ocamlbuild -package graphics windowSwapPlayer.native -use-ocamlfind
	./WindowSwapPlayer.native

intermission:
	ocamlbuild -package graphics windowIntermission.native -use-ocamlfind
	./WindowIntermission.native

intro-win:
		ocamlbuild -package graphics windowIntro.native -use-ocamlfind
	./WindowIntro.native

select-win:
		ocamlbuild -package graphics winSelect.native -use-ocamlfind
	./WinSelect.native

help-win:
		ocamlbuild -package graphics windowHelp.native -use-ocamlfind
	./WindowHelp.native

end-game:
		ocamlbuild -package graphics windowEndGame.native -use-ocamlfind
	./WindowEndGame.native