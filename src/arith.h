/************************************************************************
*									*
*		   C Prolog	arith.h					*
*		   ========	-------					*
*									*
*  By Fernando Pereira, July 1982.					*
*  EdCAAD, Dept. of Architecture, University of Edinburgh.		*
*									*
*  Based on the Prolog system written in IMP by Luis Damas for ICL 2900	*
*  computers, with some contributions by Lawrence Byrd.  Stricter types	*
*  and several extensions by Richard O'Keefe, also BACKWARDS support.	*
*									*
*  Copyright (C) 1982 Fernando Pereira, Luis Damas and Lawrence Byrd.	*
*  Copyright (C) 1984 F.Pereira, L.Damas, L.Byrd and R.A.O'Keefe.	*
*									*
************************************************************************/

/*  Internal code for the Nullary operators:				*/

#undef PI
enum { TIME = 1, HEAP, STACK, PI, LOG2 };

/* STACK is new in 1.4b -- global stack in bytes */
/* PI is new in 1.2D */
/* LOG2 is new in 1.4b -- log(2), not log10(2) */

/*  Internal codes for the Unary operators:				*/

enum { ID = 1, UMINUS, NOT, EXP, LOG, LOG10, SQRT, SIN, COS, TAN, ASIN, ACOS, ATAN, FLOOR, FLOAT };

/*  Internal codes for the Binary operators:				*/

enum { PLUS = 1, MINUS, TIMES, DIVIDE, MOD, AND, OR, LSHIFT, RSHIFT, IDIV, POW, ATAN2 };

/* Internal codes for the Comparison operators:				*/

enum { EQ, NE, LT, GT, LE, GE };
