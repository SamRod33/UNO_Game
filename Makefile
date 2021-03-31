MODULES=card player state
BYTES=$(MODULES:=.byte)
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
TEST=card_test.byte 
FINISHED_TEST=card_test_finished.byte
PKGS=ounit2

default: build

utop: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(BYTES)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

finished-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(FINISHED_TEST) && ./$(FINISHED_TEST) -runner sequential

bisect: clean test
	bisect-ppx-report html

finished-bisect: clean finished-test
	bisect-ppx-report html

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)
	
clean:
	ocamlbuild -clean
	rm -rf _coverage bisect*.coverage
