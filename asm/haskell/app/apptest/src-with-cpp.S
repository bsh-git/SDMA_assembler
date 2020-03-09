#
# assmbler test.
#   all instructions
#

#include "defs.h"
	
#define  V	(15+17-1)	/* 31 */
	
start:
	add r0, r1
	add r7, r6

	addi r0, 1
	addi r1, 42
	addi r2, 0xff

label1:
	and r1, r2
	and r6, r5

	andi r3, 2
	andi r3, 253

	andn r3, r4
	andn r5, r4

	andni r4, 3
	andni r5, 99

	asr1 r5
	asr1 r0

	bclri r6, 4
	bclri r7, V

	bdf label1
	bdf label2

	bf label1
	bf label2

	bseti r1, 7

	bsf label1
	bsf label2

	bt label1
	bt label2

	bseti r2, 19

	clrf 0
	clrf 1
	clrf 2
	clrf 3

	cmpeq r3, r4
	cmpeq r7, r5

	cmpeqi r6, 122

	cmphs r1, r7

	cmplt r2, r6

	cpshreg

	done 0   # yield
	done 1   # yieldgd
	done 2
	done 3
	done 4
	done 5

	illegal

	jmp 0x3000
	jmpr r5

	jsr SUBR
	jsrr r4

	ld r3, (r4, 31)
	ld r4, (r2, 0)

	ldf r2, 0      # MSA
	ldf r3, 4      # MDA
	ldf r4, 0xd0   # PDA


	ldi r5, 128

	ldrpc r6

	loop exit, ClearSFDF
	loop exit, ClearDF
	loop exit, ClearSF
	loop exit, KeepSFDF

	lsl1 r4
	lsr1 r5

	mov  r3, r3

	notify SetHI     # set HI
	notify ClearHE     # clear HE
	notify SetHI|ClearHE     # clear HE, set HI
	notify ClearEP     # clear EP

	or r2, r6

	ori r3, 0x55
exit:
	ret

	revb r4

	revblo r5

	ror1 r6
	rorb r7
label2:

	softbkpt

	st r0, (r0, 12)

	stf r1, 0x20  # MSA

	sub r2, r3

	subi r3, 250

	tst  r4, r7
	tsti r5, 0xaa

	xor r5, r2
	xori r6, 0x33

	yield
	yieldge

