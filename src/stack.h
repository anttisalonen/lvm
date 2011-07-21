#ifndef STACK_H
#define STACK_H

#define OPCODE_INT		0x1f
#define OPCODE_ADD		0x20
#define OPCODE_SUB		0x21
#define OPCODE_MUL		0x22
#define OPCODE_DIV		0x23
#define OPCODE_LT		0x24
#define OPCODE_LE		0x25
#define OPCODE_EQ		0x26
#define OPCODE_DEFUN_START	0x30
#define OPCODE_DEFUN_END	0x31
#define OPCODE_CALLFUN		0x32
#define OPCODE_BRANCH           0x33
#define OPCODE_BRANCHNZ         0x34
#define OPCODE_DUP		0x40
#define OPCODE_DROP		0x41
#define OPCODE_NOP		0x42
#define OPCODE_SWAP		0x43
#define OPCODE_LOAD		0x44
#define OPCODE_RET0		0x45
#define OPCODE_RET1		0x46
#define OPCODE_NEW		0x47
#define OPCODE_RSTORE		0x48
#define OPCODE_RLOAD		0x49
#define OPCODE_CALLPFUN		0x50
#define OPCODE_PFUN_ID		0x51
#define OPCODE_START_THUNK	0x52
#define OPCODE_END_THUNK	0x53
#define OPCODE_FFIDEF		0x54

/* should do for now */
/* the maximum is actually one less.
   The last slot is zero. */
#define MAX_NUM_FFI_PARAMETERS	16

#define MAGIC_00	0x6c
#define MAGIC_01	0x53
#define MAGIC_02	0x2e
#define MAGIC_03	0x46

#define CURRENT_VERSION	0x00

#define MAX_NUM_LIBS		16
#define MAX_LIBNAME_LENGTH	16

#endif

