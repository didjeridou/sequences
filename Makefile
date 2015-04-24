all: sequences SuffixArray

sequences: sequences.ml
	corebuild sequences.native

SuffixArray: SuffixArray.ml
	corebuild SuffixArray.native

clean:
	rm -rf _build *.native