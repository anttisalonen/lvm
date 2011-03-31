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

#endif

