all: quick

SRCS_DIR=src/
TEST_DIR=test/
INC=-i$(SRCS_DIR) -i$(TEST_DIR)

SRC=$(SRCS_DIR)Term.hs

QCT=$(TEST_DIR)ArbitraryTerm.hs \
    $(TEST_DIR)quickCheckTerm.hs
HCT=$(TEST_DIR)handCheckTerm.hs

OSRC=$(SRC:.hs=.o)
OQCT=$(QCT:.hs=.o)
OHCT=$(HCT:.hs=.o)

quick: $(OSRC) $(OQCT)
	ghc $(INC) --make test/quickCheckTerm.hs -o quickCheckTerm.run

hand: $(OSRC) $(OHCT)
	ghc $(INC) --make test/handCheckTerm -o handCheckTerm.run

%.o: %.hs
	ghc $(INC) -c $<

clean:
	rm -rvf */*.hi */*.o *.run
