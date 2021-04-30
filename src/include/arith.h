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

#define	TIME	1
#define	HEAP	2
#define STACK	3	/* new in 1.4b -- global stack in bytes */
#define PI	4	/* new in 1.2D */
#define	LOG2	5	/* new in 1.4b -- log(2), not log10(2) */

/*  Internal codes for the Unary operators:				*/

#define ID	1
#define UMINUS	2
#define NOT	3
#define EXP	4
#define LOG	5
#define LOG10	6
#define SQRT	7
#define SIN	8
#define COS	9
#define TAN	10
#define ASIN	11
#define ACOS	12
#define ATAN	13
#define FLOOR	14
#define FLOAT	15

/*  Internal codes for the Binary operators:				*/

#define PLUS	1
#define MINUS	2
#define TIMES	3
#define DIVIDE	4
#define MOD	5
#define AND	6
#define	OR	7
#define LSHIFT	8
#define RSHIFT	9
#define IDIV	10
#define POW	11
#define	ATAN2	12

/* Internal codes for the Comparison operators:				*/

#define EQ	0		/* =:= */
#define NE	1		/* =\= */
#define LT	2		/* < */
#define GT	3		/* > */
#define LE	4		/* =< */
#define GE	5		/* >= */

