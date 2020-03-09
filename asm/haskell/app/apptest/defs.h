#ifndef DEFS_H
#define DEFS_H

/*
 * flags for loop
 */
#define	ClearSF	2	/* clear SF, keep DF */
#define	ClearDF	1	/* clear DF, keep SF */
#define	ClearSFDF	0
#define	KeepSFDF	3

/*
 * flags for notify
 */
#define	SetHI	1
#define	ClearHE 2
#define	ClearEP	4

#define	SUBR	0x1234

#endif	/* DEFS_H */
