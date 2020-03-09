#include <stdio.h>

#define MAKETEST(exp)	printf(", textExpr \"" #exp "\" %#x\n", exp)

int
main(void)
{
	MAKETEST( 0x12345678 | 0x0f0f0f0f);
	MAKETEST( 0x12345678 & 0x0f0f0f0f);
	MAKETEST( ~0x12346789);
	MAKETEST( 0x12345678 ^ 0x0f0f0f0f);
	MAKETEST( 0x12345678 << 4);
	MAKETEST( 0x12345678 >> 4);
	MAKETEST( 0x00f0 + 1 << 8);
	MAKETEST( 0x00f0 + (1 << 8));
	MAKETEST( 0x00f0 << 1 + 8);
	MAKETEST( 0xf000 + 1 >> 8);
	MAKETEST( 0x1234 & 0xff00 | 0x0033);
	MAKETEST( 0x1234 & 0xff00 ^ 0xff00);
	MAKETEST( 0x1234 & 0x00ff << 8);

	return 0;
}
