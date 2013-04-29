draw: draw.lhs
	ghc --make draw.lhs
	./draw -o pic.svg -w 400

	rm draw.o draw.hi draw