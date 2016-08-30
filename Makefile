all:
	obuild configure
	obuild build lib-ptset

test:
	obuild configure
	obuild build exe-test
	./test

doc:
	mkdir -p doc
	ocamldoc -d doc/ -html ptset.mli

install: all
	obuild install

uninstall:
	ocamlfind -remove ptset

clean:
	obuild clean
	rm -rf doc
