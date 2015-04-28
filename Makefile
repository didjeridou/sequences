all: sequences

c: clean all

sequences: sequences.ml
	corebuild sequences.native

CAMLDOC = ocamldoc

# Source and Object files
SOURCES = \
	DNASequence.ml \
	sequences.ml \
	SuffixArray.ml \


OBJECTS = $(SOURCES:.ml=.cmo)

# DocGen

doc: $(OBJECTS)
	$(CAMLDOC) -html $(SOURCES)


clean:
	rm -rf _build *.native *.cmi *.cmo

