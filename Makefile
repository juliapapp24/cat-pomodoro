.PHONY: test check

build:
	dune build
	
run:
	dune exec src/main.exe

zip:
	rm -f demo.zip
	zip -r demo.zip . -x@exclude.lst

clean:
	dune clean
	rm -f demo.zip

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

