/*--------------------------------------------------------------------*/
/* CPU6502                                                            */
/* by GienekP                                                         */
/* (c) 2024                                                           */
/*--------------------------------------------------------------------*/
#include <stdio.h>
#include <time.h>
/*--------------------------------------------------------------------*/
#define CFLAG 0x01
#define ZFLAG 0x02
#define IFLAG 0x04
#define DFLAG 0x08
#define BFLAG 0x10
#define VFLAG 0x40
#define NFLAG 0x80
/*--------------------------------------------------------------------*/
typedef unsigned char U8;
typedef unsigned short U16;
typedef unsigned int U32;
/*--------------------------------------------------------------------*/
void setN(U8 *F, U8 condition)
{
	if (condition) {(*F)|=NFLAG;} else {(*F)&=(NFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
void setV(U8 *F, U8 condition)
{
	if (condition) {(*F)|=VFLAG;} else {(*F)&=(VFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
void setD(U8 *F, U8 condition)
{
	if (condition) {(*F)|=DFLAG;} else {(*F)&=(DFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
void setI(U8 *F, U8 condition)
{
	if (condition) {(*F)|=IFLAG;} else {(*F)&=(IFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
void setZ(U8 *F, U8 condition)
{
	if (condition) {(*F)|=ZFLAG;} else {(*F)&=(ZFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
void setC(U8 *F, U8 condition)
{
	if (condition) {(*F)|=CFLAG;} else {(*F)&=(CFLAG ^ 0xFF);};
};
/*--------------------------------------------------------------------*/
U16 adr8(U8 *mem, U16 *PC )
{
	U16 ret;
	ret=mem[*PC];
	return ret;
}
/*--------------------------------------------------------------------*/
U16 adr16(U8 *mem, U16 *PC)
{
	U16 ret;
	ret=mem[(*PC)+1];
	ret<<=8;
	ret|=mem[(*PC)];
	return ret;
}
/*--------------------------------------------------------------------*/
void store(U8 *mem, U8 *S, U8 val)
{
	U16 addr=0x0100;
	addr+=(*S);
	mem[addr]=val;
	(*S)--;
}
/*--------------------------------------------------------------------*/
U8 restore(U8 *mem, U8 *S)
{
	U8 ret;
	U16 addr=0x0100;
	addr+=(*S);
	ret=mem[addr];
	(*S)++;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *immediate(U8 *mem, U16 *PC)
{
	U8 *ret;
	ret=&mem[*PC];
	(*PC)++;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *zeropage(U8 *mem, U16 *PC)
{
	U8 *ret;
	ret=&mem[adr8(mem,PC)];
	(*PC)++;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *zeropageQ(U8 *mem, U16 *PC, U8 *Q)
{
	U8 *ret;
	U16 addr;
	addr=adr8(mem,PC);
	addr+=(*Q);
	ret=&mem[addr];
	(*PC)++;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *absolute(U8 *mem, U16 *PC)
{
	U8 *ret;
	ret=&mem[adr16(mem,PC)];
	(*PC)+=2;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *absoluteQ(U8 *mem, U16 *PC, U8 *Q)
{
	U8 *ret;
	U16 addr;
	addr=adr16(mem,PC);
	addr+=(*Q);
	ret=&mem[addr];
	(*PC)+=2;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *indirectX(U8 *mem, U16 *PC, U8 *X)
{
	U8 *ret;
	U16 addr;
	addr=adr8(mem,PC);
	addr+=(*X);
	addr=adr16(mem,&addr);
	ret=&mem[addr];
	(*PC)+=1;
	return ret;
}
/*--------------------------------------------------------------------*/
U8 *indirectY(U8 *mem, U16 *PC, U8 *Y)
{
	U8 *ret;
	U16 addr;
	addr=adr8(mem,PC);
	addr=adr16(mem,&addr);
	addr+=(*Y);
	ret=&mem[addr];
	(*PC)+=1;
	return ret;
}
/*--------------------------------------------------------------------*/
void adc(U8 *val, U8 *A, U8 *F)
{
	U16 C,sum=( (*F) & CFLAG);
	sum+=(*val);
	sum+=(*A);
	C=(sum>>8);
	setC(F,C);
	*A=(sum&0xFF);
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void and(U8 *val, U8 *A, U8 *F)
{
	(*A)&=(*val);
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void asl(U8 *val, U8 *F)
{
	setC(F,(*val)&0x80);
	(*val)<<=1;
	setZ(F,(*val)==0);
	setN(F,((*val) & NFLAG));
}
/*--------------------------------------------------------------------*/
void branch(U8 *mem, U16 *PC, U8 condition)
{
	if (condition)
	{
		signed char off=mem[*PC];
		(*PC)++;
		(*PC)+=off;
	}
	else
	{
		(*PC)++;
	};
}
/*--------------------------------------------------------------------*/
void bit(U8 *val, U8 *A, U8 *F)
{
	U8 bit=((*val) & (*A));
	(*F)&=0x3F;
	(*F)|=((*val) & 0xC0);
	setZ(F,bit==0);
}
/*--------------------------------------------------------------------*/
void compare(U8 *oper, U8 *reg, U8 *F)
{
	U8 test=((*reg)-(*oper));
	if (test==0)
	{
		*F|=(ZFLAG | CFLAG);
		*F&=(NFLAG ^ 0xFF);
	}
	else
	{
		setC(F,((*reg)>(*oper)));
		setZ(F,0);
		setN(F,(test & NFLAG));
	};
}
/*--------------------------------------------------------------------*/
void decrement(U8 *val, U8 *F)
{
	(*val)--;
	setZ(F,(*val)==0);
	setN(F,((*val) & NFLAG));
}
/*--------------------------------------------------------------------*/
void eor(U8 *val, U8 *A, U8 *F)
{
	(*A)^=(*val);
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void increment(U8 *val, U8 *F)
{
	(*val)++;
	setZ(F,(*val)==0);
	setN(F,((*val) & NFLAG));
}
/*--------------------------------------------------------------------*/
void jsr(U8 *mem, U16 *PC, U8 *S)
{
	U16 back=(*PC)+1;
	(*PC)=adr16(mem,PC);
	store(mem,S,(back >> 8));
	store(mem,S,(back & 0xFF));	
}
/*--------------------------------------------------------------------*/
void load(U8 *val, U8 *dest, U8 *F)
{
	(*dest)=(*val);
	setZ(F,(*dest)==0);
	setN(F,((*dest) & NFLAG));
}
/*--------------------------------------------------------------------*/
void lsr(U8 *val, U8 *F)
{
	setC(F,(*val)&0x01);
	(*val)>>=1;
	setZ(F,(*val)==0);
	setN(F,0);
}
/*--------------------------------------------------------------------*/
void ora(U8 *val, U8 *A, U8 *F)
{
	(*A)|=(*val);
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void pla(U8 *mem, U8 *S, U8 *A, U8 *F)
{
	(*A)=restore(mem,S);
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void rol(U8 *val, U8 *F)
{
	U8 c=((*F) & CFLAG);
	setC(F,(*val) & 0x80);
	(*val)<<=1;
	(*val)|=c;
	setZ(F,(*val)==0);
	setN(F,0);
}
/*--------------------------------------------------------------------*/
void ror(U8 *val, U8 *F)
{
	U8 c=(((*F) & CFLAG)<<7);
	setC(F,(*val) & 0x01);
	(*val)>>=1;
	(*val)|=c;
	setZ(F,(*val)==0);
	setN(F,0);
}
/*--------------------------------------------------------------------*/
void rts(U8 *mem, U16 *PC, U8 *S)
{
	U16 addr1=restore(mem,S);
	U16 addr2=restore(mem,S);
	addr2<<=8;
	(*PC)=((addr1 | addr2)+1);
}
/*--------------------------------------------------------------------*/
void rti(U8 *mem, U16 *PC, U8 *S, U8 *F)
{
	U16 addr1,addr2;
	(*S)=(restore(mem,S) & 0xCF);
	addr1=restore(mem,S);
	addr2=restore(mem,S);
	addr2<<=8;
	(*PC)=(addr1 | addr2);
}
/*--------------------------------------------------------------------*/
void sbc(U8 *val, U8 *A, U8 *F)
{
	U8 C=(( (*F) & CFLAG)^0x01);
	setC(F,(*A)>(*val));
	(*A)-=(*val);
	(*A)-=C;
	setZ(F,(*A)==0);
	setN(F,((*A) & NFLAG));
}
/*--------------------------------------------------------------------*/
void save(U8 *val, U8 *source)
{
	(*val)=(*source);
}
/*--------------------------------------------------------------------*/
void transfer(U8 *source, U8 *dest, U8 *F)
{
	(*dest)=(*source);
	setZ(F,(*dest)==0);
	setN(F,((*dest) & NFLAG));
}
/*--------------------------------------------------------------------*/
void cpu6502(U8 *mem, U16 *PC, U8 *F, U8 *S, U8 *A, U8 *X, U8 *Y)
{
	U8 inst=mem[(*PC)]; //printf("%x\n",inst);
	(*PC)++;
	switch (inst)
	{

/* ADC #oper 	*/	case 0x69: { adc(immediate(mem,PC),A,F); } break;
/* ADC zero 	*/	case 0x65: { adc(zeropage(mem,PC),A,F); } break;
/* ADC zero,X 	*/	case 0x75: { adc(zeropageQ(mem,PC,X),A,F); } break;
/* ADC oper 	*/	case 0x6D: { adc(absolute(mem,PC),A,F); } break;
/* ADC oper,X 	*/	case 0x7D: { adc(absoluteQ(mem,PC,X),A,F); } break;
/* ADC oper,Y 	*/	case 0x79: { adc(absoluteQ(mem,PC,Y),A,F); } break;
/* ADC (oper,X)	*/	case 0x61: { adc(indirectX(mem,PC,X),A,F); } break;
/* ADC (oper),Y	*/	case 0x71: { adc(indirectY(mem,PC,Y),A,F); } break;

/* AND #oper 	*/	case 0x29: { and(immediate(mem,PC),A,F); } break;
/* AND zero 	*/	case 0x25: { and(zeropage(mem,PC),A,F); } break;
/* AND zero,X 	*/	case 0x35: { and(zeropageQ(mem,PC,X),A,F); } break;
/* AND oper 	*/	case 0x2D: { and(absolute(mem,PC),A,F); } break;
/* AND oper,X 	*/	case 0x3D: { and(absoluteQ(mem,PC,X),A,F); } break;
/* AND oper,Y 	*/	case 0x39: { and(absoluteQ(mem,PC,Y),A,F); } break;
/* AND (oper,X)	*/	case 0x21: { and(indirectX(mem,PC,X),A,F); } break;
/* AND (oper),Y	*/	case 0x31: { and(indirectY(mem,PC,Y),A,F); } break;

/* ASL A	 	*/	case 0x0A: { asl(A,F); } break;
/* ASL zero 	*/	case 0x06: { asl(zeropage(mem,PC),F); } break;
/* ASL zero,X 	*/	case 0x16: { asl(zeropageQ(mem,PC,X),F); } break;
/* ASL oper 	*/	case 0x0E: { asl(absolute(mem,PC),F); } break;
/* ASL oper,X 	*/	case 0x1E: { asl(absoluteQ(mem,PC,X),F); } break;

/* BCC oper		*/	case 0x90: { branch(mem,PC,!((*F) & CFLAG)); } break;
/* BCS oper		*/	case 0xB0: { branch(mem,PC,((*F) & CFLAG)); } break;
/* BNE oper		*/	case 0xD0: { branch(mem,PC,!((*F) & ZFLAG)); } break;
/* BEQ oper		*/	case 0xF0: { branch(mem,PC,((*F) & ZFLAG)); } break;
/* BPL oper		*/	case 0x10: { branch(mem,PC,!((*F) & NFLAG)); } break;
/* BMI oper		*/	case 0x30: { branch(mem,PC,((*F) & NFLAG)); } break;
/* BVC oper		*/	case 0x50: { branch(mem,PC,!((*F) & VFLAG)); } break;
/* BVS oper		*/	case 0x70: { branch(mem,PC,((*F) & VFLAG)); } break;

/* BIT zero		*/	case 0x24: { bit(zeropage(mem,PC),A,F); } break;
/* BIT oper		*/	case 0x2C: { bit(absolute(mem,PC),A,F); } break;

/* BRK			*/	case 0x00: { } break;

/* CLC			*/	case 0x18: { setC(F,0); } break;
/* CLD			*/	case 0xD8: { setD(F,0); } break;
/* CLI			*/	case 0x58: { setI(F,0); } break;
/* CLV			*/	case 0xB8: { setV(F,0); } break;

/* CMP #oper 	*/	case 0xC9: { compare(immediate(mem,PC),A,F); } break;
/* CMP zero 	*/	case 0xC5: { compare(zeropage(mem,PC),A,F); } break;
/* CMP zero,X 	*/	case 0xD5: { compare(zeropageQ(mem,PC,X),A,F); } break;
/* CMP oper 	*/	case 0xCD: { compare(absolute(mem,PC),A,F); } break;
/* CMP oper,X 	*/	case 0xDD: { compare(absoluteQ(mem,PC,X),A,F); } break;
/* CMP oper,Y 	*/	case 0xD9: { compare(absoluteQ(mem,PC,Y),A,F); } break;
/* CMP (oper,X)	*/	case 0xC1: { compare(indirectX(mem,PC,X),A,F); } break;
/* CMP (oper),Y	*/	case 0xD1: { compare(indirectY(mem,PC,Y),A,F); } break;

/* CPX #oper 	*/	case 0xE0: { compare(immediate(mem,PC),X,F); } break;
/* CPX zero 	*/	case 0xE4: { compare(zeropage(mem,PC),X,F); } break;
/* CPX oper 	*/	case 0xEC: { compare(absolute(mem,PC),X,F); } break;

/* CPY #oper 	*/	case 0xC0: { compare(immediate(mem,PC),Y,F); } break;
/* CPY zero 	*/	case 0xC4: { compare(zeropage(mem,PC),Y,F); } break;
/* CPY oper 	*/	case 0xCC: { compare(absolute(mem,PC),Y,F); } break;

/* DEC zero 	*/	case 0xC6: { decrement(zeropage(mem,PC),F); } break;
/* DEC zero,X 	*/	case 0xD6: { decrement(zeropageQ(mem,PC,X),F); } break;
/* DEC oper 	*/	case 0xCE: { decrement(absolute(mem,PC),F); } break;
/* DEC oper,X 	*/	case 0xDE: { decrement(absoluteQ(mem,PC,X),F); } break;

/* DEX			*/	case 0xCA: { decrement(X,F); } break;
/* DEY			*/	case 0x88: { decrement(Y,F); } break;

/* EOR #oper 	*/	case 0x49: { eor(immediate(mem,PC),A,F); } break;
/* EOR zero 	*/	case 0x45: { eor(zeropage(mem,PC),A,F); } break;
/* EOR zero,X 	*/	case 0x55: { eor(zeropageQ(mem,PC,X),A,F); } break;
/* EOR oper 	*/	case 0x4D: { eor(absolute(mem,PC),A,F); } break;
/* EOR oper,X 	*/	case 0x5D: { eor(absoluteQ(mem,PC,X),A,F); } break;
/* EOR oper,Y 	*/	case 0x59: { eor(absoluteQ(mem,PC,Y),A,F); } break;
/* EOR (oper,X)	*/	case 0x41: { eor(indirectX(mem,PC,X),A,F); } break;
/* EOR (oper),Y	*/	case 0x51: { eor(indirectY(mem,PC,Y),A,F); } break;

/* INC zero 	*/	case 0xE6: { increment(zeropage(mem,PC),F); } break;
/* INC zero,X 	*/	case 0xF6: { increment(zeropageQ(mem,PC,X),F); } break;
/* INC oper 	*/	case 0xEE: { increment(absolute(mem,PC),F); } break;
/* INC oper,X 	*/	case 0xFE: { increment(absoluteQ(mem,PC,X),F); } break;

/* INX			*/	case 0xE8: { increment(X,F); } break;
/* INY			*/	case 0xC8: { increment(Y,F); } break;

/* JMP oper 	*/	case 0x4C: { (*PC)=adr16(mem,PC); } break;
/* JMP (oper) 	*/	case 0x6C: { (*PC)=adr16(mem,PC); (*PC)=adr16(mem,PC); } break;

/* JSR oper 	*/	case 0x20: { jsr(mem,PC,S); } break;

/* LDA #oper 	*/	case 0xA9: { load(immediate(mem,PC),A,F); } break;
/* LDA zero 	*/	case 0xA5: { load(zeropage(mem,PC),A,F); } break;
/* LDA zero,X 	*/	case 0xB5: { load(zeropageQ(mem,PC,X),A,F); } break;
/* LDA oper 	*/	case 0xAD: { load(absolute(mem,PC),A,F); } break;
/* LDA oper,X 	*/	case 0xBD: { load(absoluteQ(mem,PC,X),A,F); } break;
/* LDA oper,Y 	*/	case 0xB9: { load(absoluteQ(mem,PC,Y),A,F); } break;
/* LDA (oper,X)	*/	case 0xA1: { load(indirectX(mem,PC,X),A,F); } break;
/* LDA (oper),Y	*/	case 0xB1: { load(indirectY(mem,PC,Y),A,F); } break;

/* LDX #oper 	*/	case 0xA2: { load(immediate(mem,PC),X,F); } break;
/* LDX zero 	*/	case 0xA6: { load(zeropage(mem,PC),X,F); } break;
/* LDX zero,Y 	*/	case 0xB6: { load(zeropageQ(mem,PC,Y),X,F); } break;
/* LDX oper 	*/	case 0xAE: { load(absolute(mem,PC),X,F); } break;
/* LDX oper,Y 	*/	case 0xBE: { load(absoluteQ(mem,PC,Y),X,F); } break;

/* LDY #oper 	*/	case 0xA0: { load(immediate(mem,PC),Y,F); } break;
/* LDY zero 	*/	case 0xA4: { load(zeropage(mem,PC),Y,F); } break;
/* LDY zero,X 	*/	case 0xB4: { load(zeropageQ(mem,PC,X),Y,F); } break;
/* LDY oper 	*/	case 0xAC: { load(absolute(mem,PC),Y,F); } break;
/* LDY oper,X 	*/	case 0xBC: { load(absoluteQ(mem,PC,X),Y,F); } break;

/* LSR A	 	*/	case 0x4A: { lsr(A,F); } break;
/* LSR zero 	*/	case 0x46: { lsr(zeropage(mem,PC),F); } break;
/* LSR zero,X 	*/	case 0x56: { lsr(zeropageQ(mem,PC,X),F); } break;
/* LSR oper 	*/	case 0x4E: { lsr(absolute(mem,PC),F); } break;
/* LSR oper,X 	*/	case 0x5E: { lsr(absoluteQ(mem,PC,X),F); } break;

/* NOP 			*/	case 0xEA: { } break;

/* ORA #oper 	*/	case 0x09: { ora(immediate(mem,PC),A,F); } break;
/* ORA zero 	*/	case 0x05: { ora(zeropage(mem,PC),A,F); } break;
/* ORA zero,X 	*/	case 0x15: { ora(zeropageQ(mem,PC,X),A,F); } break;
/* ORA oper 	*/	case 0x0D: { ora(absolute(mem,PC),A,F); } break;
/* ORA oper,X 	*/	case 0x1D: { ora(absoluteQ(mem,PC,X),A,F); } break;
/* ORA oper,Y 	*/	case 0x19: { ora(absoluteQ(mem,PC,Y),A,F); } break;
/* ORA (oper,X)	*/	case 0x01: { ora(indirectX(mem,PC,X),A,F); } break;
/* ORA (oper),Y	*/	case 0x11: { ora(indirectY(mem,PC,Y),A,F); } break;

/* PHA 			*/	case 0x48: { store(mem,S,*A); } break;
/* PHP 			*/	case 0x08: { store(mem,S,((*S) | BFLAG)); } break;
/* PLA 			*/	case 0x68: { pla(mem,S,A,F); } break;
/* PLP 			*/	case 0x28: { (*F)=(restore(mem,S) & 0xCF); } break;

/* ROL A	 	*/	case 0x2A: { rol(A,F); } break;
/* ROL zero 	*/	case 0x26: { rol(zeropage(mem,PC),F); } break;
/* ROL zero,X 	*/	case 0x36: { rol(zeropageQ(mem,PC,X),F); } break;
/* ROL oper 	*/	case 0x2E: { rol(absolute(mem,PC),F); } break;
/* ROL oper,X 	*/	case 0x3E: { rol(absoluteQ(mem,PC,X),F); } break;

/* ROR A	 	*/	case 0x6A: { ror(A,F); } break;
/* ROR zero 	*/	case 0x66: { ror(zeropage(mem,PC),F); } break;
/* ROR zero,X 	*/	case 0x76: { ror(zeropageQ(mem,PC,X),F); } break;
/* ROR oper 	*/	case 0x6E: { ror(absolute(mem,PC),F); } break;
/* ROR oper,X 	*/	case 0x7E: { ror(absoluteQ(mem,PC,X),F); } break;

/* RTI 			*/	case 0x40: { rti(mem,PC,S,F); } break;
/* RTS 			*/	case 0x60: { rts(mem,PC,S); } break;

/* SBC #oper 	*/	case 0xE9: { sbc(immediate(mem,PC),A,F); } break;
/* SBC zero 	*/	case 0xE5: { sbc(zeropage(mem,PC),A,F); } break;
/* SBC zero,X 	*/	case 0xF5: { sbc(zeropageQ(mem,PC,X),A,F); } break;
/* SBC oper 	*/	case 0xED: { sbc(absolute(mem,PC),A,F); } break;
/* SBC oper,X 	*/	case 0xFD: { sbc(absoluteQ(mem,PC,X),A,F); } break;
/* SBC oper,Y 	*/	case 0xF9: { sbc(absoluteQ(mem,PC,Y),A,F); } break;
/* SBC (oper,X)	*/	case 0xE1: { sbc(indirectX(mem,PC,X),A,F); } break;
/* SBC (oper),Y	*/	case 0xF1: { sbc(indirectY(mem,PC,Y),A,F); } break;

/* SEC			*/	case 0x38: { setC(F,1); } break;
/* SED			*/	case 0xF8: { setD(F,1); } break;
/* SEI			*/	case 0x78: { setI(F,1); } break;

/* STA zero 	*/	case 0x85: { save(zeropage(mem,PC),A); } break;
/* STA zero,X 	*/	case 0x95: { save(zeropageQ(mem,PC,X),A); } break;
/* STA oper 	*/	case 0x8D: { save(absolute(mem,PC),A); } break;
/* STA oper,X 	*/	case 0x9D: { save(absoluteQ(mem,PC,X),A); } break;
/* STA oper,Y 	*/	case 0x99: { save(absoluteQ(mem,PC,Y),A); } break;
/* STA (oper,X)	*/	case 0x81: { save(indirectX(mem,PC,X),A); } break;
/* STA (oper),Y	*/	case 0x91: { save(indirectY(mem,PC,Y),A); } break;

/* STX zero 	*/	case 0x86: { save(zeropage(mem,PC),X); } break;
/* STX zero,Y 	*/	case 0x96: { save(zeropageQ(mem,PC,Y),X); } break;
/* STX oper 	*/	case 0x8E: { save(absolute(mem,PC),X); } break;

/* STY zero 	*/	case 0x84: { save(zeropage(mem,PC),Y); } break;
/* STY zero,X 	*/	case 0x94: { save(zeropageQ(mem,PC,X),Y); } break;
/* STY oper 	*/	case 0x8C: { save(absolute(mem,PC),Y); } break;

/* TAX		 	*/	case 0xAA: { transfer(A,X,F); } break;
/* TAY		 	*/	case 0xA8: { transfer(A,Y,F); } break;
/* TSX		 	*/	case 0xBA: { transfer(S,X,F); } break;
/* TXA		 	*/	case 0x8A: { transfer(X,A,F); } break;
/* TXS		 	*/	case 0x9A: { (*S)=(*X); } break;
/* TYA		 	*/	case 0x98: { transfer(Y,A,F); } break;

/* UNKNOWN		*/	default: {printf("[%02X]\n",inst);} break;
	};
}
/*--------------------------------------------------------------------*/
int main( int argc, char* argv[] )
{	
	U8 memory[64*1024]={0xA9,0x24, 0x38, 0xE9,0x25, 0xA8, 0xA9,0x05, 0xE9,0x55, 0xAa, 0x4C,0x00,0x00};
	U16 PC=0;
	U8 F=0x00,S=0xFF,A=0,X=0x00,Y=0x00;
	int i, start, end;
	printf("CPU6502\n");
	printf("PC=$%04X F=$%02X S=$%02X A=$%02X X=$%02X Y=$%02X\n\n",PC,F,S,A,X,Y);
	start=clock();
	for (i=0; i<100000000; i++)
	{
		cpu6502(memory,&PC,&F,&S,&A,&X,&Y);
		//printf("PC=$%04X F=$%02X S=$%02X A=$%02X X=$%02X Y=$%02X\n",PC,F,S,A,X,Y);
	};
	end=clock();
	printf("\nPC=$%04X F=$%02X S=$%02X A=$%02X X=$%02X Y=$%02X\n\n",PC,F,S,A,X,Y);
	printf("Time=%f[s]\n",((float)(end - start))/CLOCKS_PER_SEC);
	return 0;
}
/*--------------------------------------------------------------------*/
