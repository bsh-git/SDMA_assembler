PERLOPT=-I$(SDMA_ASM_PL_DIR)
SDMA_ASM_PL=$(SDMA_ASM_PL_DIR)/sdma_asm.pl


test1: expect/allinstructions.dat allinstructions.asm
	stack run -- --format data allinstructions.asm | tee output
	diff -u expect/allinstructions.dat output


expect/allinstructions.dat: expect/allinstructions.c c2dat.sh
	./c2dat.sh $< > tmp
	mv tmp $@

expect/allinstructions.c: allinstructions.asm Makefile
	perl $(PERLOPT) $(SDMA_ASM_PL) $< | sed -n '/CUT HERE/,$$p' | sed '/CUT HERE/d' > tmp
	mv tmp $@

