# Sdmaasm

This is an assembler for SDMA found on (freescale's then) NXP's i.MX
series SoCs.

SDMA is a 16-bit RISC micro controller that is designed to perform DMA tasks.

## Usage

sdmaasm [*options*] [*inputfile*]

options are:

- -a ADDRESS, --loadaddr ADDRESS

locate the program to ADDRESS.  Position independent codes can be assembled without this options.
- -F FORMAT, --format FORMAT

Output format.
       - c (default): generate a C program
       - linux: generate a C program compatible with sdma_asm.pl
       - data: outputs sequence of 32-bit hexadecimal numbers

if *inputfile* is ommitted, stdin is read.

output is always written to stdout.

## using with C preprocessor
Sdmaasm is c-preprocessor friendly.  You can write `#define` or `#include` directives in your assembler source,
and run sdmaasm as `cpp --traditional source.asm | sdmaasm > output.c`

## directives
Following directives are supported.

### .dc
.dc[*size*] *val1*, *val2*, ...

*size* can be one of .b, .w, and .l which are for byte, 16-bit word and 32-bit word respectively.

### .align
.align *n*

*n* should be a number that is power of 2.

## General purpose registers
General purpose registers are always specified as r0, r1, ... r7.
Bare numbers can not be recognized as general purpose registers, unlike in freescale's manual.


## difference to sdma_asm.pl
- c-preprocessor friendly.
- Basic arithmetic can be used in operands, including `+ - * / % & | ^ ~ << >>`
- Local labels can be used as in Unix assemblers such as GNU
  assembler.
- `jmp Label` or `jsr Label` can be used with load address specified
  by --loadaddr option.





