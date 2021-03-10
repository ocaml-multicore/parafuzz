.PHONY: clean all run_test

all:
		dune build @install --root=.

clean:
		dune clean --root=.

run_test:
		dune exec test/test_parafuzz.exe --root=.
		dune exec test/test_range.exe --root=.
