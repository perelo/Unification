all: test

SRCS_DIR=src/
TEST_DIR=test/
INC=-i$(SRCS_DIR) -i$(TEST_DIR)

SRC=$(SRCS_DIR)Term.hs
TST=$(TEST_DIR)ArbitraryTerm.hs \
    $(TEST_DIR)testTerm.hs

OSRC=$(SRC:.hs=.o)
OTST=$(TST:.hs=.o)

test: $(OSRC) $(OTST)
	ghc $(INC) --make test/testTerm -o testTerm.run

%.o: %.hs
	ghc $(INC) -c $<

clean:
	rm -rvf */*.hi */*.o *.run
