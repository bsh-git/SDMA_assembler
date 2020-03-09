PERLOPT=-I$(SDMA_ASM_PL_DIR)
SDMA_ASM_PL=$(SDMA_ASM_PL_DIR)/sdma_asm.pl


alltests: test01 \
	test02 \
	test03-00 test03-01 test03-02

test01: expect/allinstructions.dat allinstructions.asm
	stack run -- --format data allinstructions.asm | tee output
	diff -u expect/allinstructions.dat output


test02: expect/allinstructions.dat src-with-cpp.S
	cpp --traditional src-with-cpp.S | stack run -- --format data > output
	diff -u expect/allinstructions.dat output

test03-00: expect/address-test-00.dat
	stack run -- --format data address-test-00.asm > output
	diff -u expect/address-test-00.dat output

test03-01: expect/address-test-00.dat address-test-01.asm
	stack run -- --format data address-test-01.asm > output
	diff -u expect/address-test-00.dat output

test03-02: expect/address-test-00.dat address-test-02.asm
	stack run -- --format data address-test-02.asm > output
	diff -u expect/address-test-00.dat output

test03-03: expect/address-test-00.dat address-test-00.asm
	(cat address-test-00.asm; echo "jmp label1; jsr label2") | stack run -- -a 0x0400 --format data > output
	(cat expect/address-test-00.dat; echo 8403c404) | diff -u - output


expect/allinstructions.dat: expect/allinstructions.c c2dat.sh
	./c2dat.sh $< > tmp
	mv tmp $@

expect/allinstructions.c: allinstructions.asm Makefile
	[ -f "${SDMA_ASM_PL}" ] || exit 1
	perl $(PERLOPT) $(SDMA_ASM_PL) $< | sed -n '/CUT HERE/,$$p' | sed '/CUT HERE/d' > tmp
	[ ! -z tmp ] || exit 1
	mv tmp $@


expect/address-test-00.dat: expect/address-test-00.c c2dat.sh
	./c2dat.sh $< > tmp
	mv tmp $@

expect/address-test-00.c: address-test-00.asm
	[ -f "${SDMA_ASM_PL}" ] || exit 1
	perl $(PERLOPT) $(SDMA_ASM_PL) $< | sed -n '/CUT HERE/,$$p' | sed '/CUT HERE/d' > tmp
	[ ! -z tmp ] || exit 1
	mv tmp $@
