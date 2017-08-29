all:
	obuild configure
	obuild build lib-ptmap

test: unit_tests.ml
	obuild configure --enable-tests
	obuild build
	obuild test --output

doc:
	mkdir -p doc
	ocamldoc -d doc/ -html ptmap.mli

install: all
	obuild install

uninstall:
	ocamlfind -remove ptmap

clean:
	obuild clean
	rm -rf doc qtest.targets.log oUnit-anon.cache unit_tests.ml

unit_tests.ml: ptmap.ml
	qtest extract ptmap.ml -o unit_tests.ml
