ELM = elm
OUTPUT = GeneticSpaceInvaders.html # For Rails asset pipeline, but could be anything
SRCS = GeneticSpaceInvaders.elm
TARGETS = $(SRCS:.elm=.html)
ELM_STUFF = elm-stuff/exact-dependencies.json

all: $(TARGETS)

%.html: src/%.elm $(ELM_STUFF)
	$(ELM) make $< --output $(OUTPUT)

$(ELM_STUFF): elm-package.json
	$(ELM) package install

clean:
	echo "Removing build artifacts..."
	rm -rf ./elm-stuff/build-artifacts
	rm -rf ./elm-linter/build-artifacts