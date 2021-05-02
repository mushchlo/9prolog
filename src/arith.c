/************************************************************************
*									*
*		   C Prolog      arith.c				*
*		   ========	 -------				*
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

/* Evaluate arithmetic expressions */

#include "pl.h"
#include "arith.h"
#include <math.h>
#include <errno.h>

enum { NEVER, SOMETIMES, ALWAYS };

int AllFloat = TRUE;		/* may only be TRUE or FALSE */


void ArithError(s)		/* also called by FPE trap in sysbits.c */
    char *s;
    {
	ErrorMess = s;
	Event(ARITH_ERROR);
    }


 void NotInt(fn)
    FUNCTORP fn;
    {
	Ignore sprint(OutBuf, "the arguments of '%s'/%d must be integers",
		fn->atoffe->stofae, fn->arityoffe);
	ArithError(OutBuf);
    }


 void NotOp(fn)
    FUNCTORP fn;
    {
	if (fn->arityoffe == 0)
	    Ignore sprint(OutBuf, "%s is not a number", fn->atoffe->stofae);
	else
	    Ignore sprint(OutBuf, "%s/%d is not an arithmetic function",
		fn->atoffe->stofae, fn->arityoffe);
	ArithError(OutBuf);
    }


 double ffail()
 {
	ArithError("internal error - undefined operator");
	return 0.0;
 }


 double (*UFloat[])() =
    {
	ffail,
	ffail,
	ffail,
	ffail,
	exp,
	log,
	log10,
	sqrt,
	sin,
	cos,
	tan,
	asin,
	acos,
	atan,
	floor,
	ffail
    };
 char fl_unary[] =
    {				/* code arity function */
	NEVER,			/*    0     - UNUSABLE */
	NEVER,			/*    1     1        + */
	SOMETIMES,		/*    2     1        - */
	NEVER,			/*    3     1        \ */
	ALWAYS,			/*    4     1      exp */
	ALWAYS,			/*    5     1      log */
	ALWAYS,			/*    6     1    log10 */
	ALWAYS,			/*    7     1     sqrt */
	ALWAYS,			/*    8     1      sin */
	ALWAYS,			/*    9     1      cos */
	ALWAYS,			/*   10     1      tan */
	ALWAYS,			/*   11     1     asin */
	ALWAYS,			/*   12     1     acos */
	ALWAYS,			/*   13     1     atan */
	SOMETIMES,		/*   14     1    floor */
	ALWAYS			/*   15     1    float */
    };
 char fl_binary[] =
    {				/* code arity function */
	SOMETIMES,		/*    0     - UNUSABLE */
	SOMETIMES,		/*    1     2        + */
	SOMETIMES,		/*    2     2        - */
	SOMETIMES,		/*    3     2        * */
	SOMETIMES,		/*    4     2        / */
	NEVER,			/*    5     2      mod */
	NEVER,			/*    6     2       /\ */
	NEVER,			/*    7     2       \/ */
	NEVER,			/*    8     2       << */
	NEVER,			/*    9     2       >> */
	NEVER,			/*   10     2      div */
	ALWAYS,			/*   11     2        ^ */
	ALWAYS,			/*   12     2    atan2 */
	NEVER,			/*   13     2    SPARE */
	NEVER,			/*   14     2    SPARE */
	NEVER			/*   15     2    SPARE */
    };


/*  A Value is either a long integer or a double precision  float.   The
    old  declaration was struct { union {long,double}, char }, and these
    structures used to be returned from functions.  But some C compilers
    do not support structure returning or even structure assignment.  If
    yours is such a compiler, you will need to define  NO_STRUCT_ASSIGN.
    That  copies  fields  across.   But we have no guarantee that double
    x,y; x = y; will move y to x with no  change  of  bit  pattern.   We
    could  use  strncpy(),  but that would be silly.  So we have to copy
    the fields separately.  Mind you, with such an archaic compiler  you
    may have to use someone else's "cpp"...
*/
#if	NO_STRUCT_ASSIGN

typedef struct Value
    {
	double AsFloat;
	ProLong AsInt;
	char Float;
    }	Value;

#define	CopyValue(x,y) x.AsFloat=y.AsFloat,x.AsInt=y.AsInt,x.Float=y.Float

#else  !NO_STRUCT_ASSIGN

typedef struct Value
    {
	union {ProLong asInt; double asFloat;} val;
	char Float;
    } Value;

#define	AsInt	val.asInt
#define AsFloat	val.asFloat
#define	CopyValue(x,y) x=y

#endif	NO_STRUCT_ASSIGN

Value reg;			/* the value "register" */


int Narrow(f, i)		/* also used in rewrite.c */
    double f;
	ProLong *i;
    {
	if (f < MinInt || f > MaxInt || (ProLong)f != f) return FALSE;
	*i = (ProLong)f;
	return TRUE;
    }


 int ForceInt(val)
    register Value *val;
    {
	if (val->Float) {
	    if (val->AsFloat < MinInt || val->AsFloat > MaxInt ||
		(ProLong)(val->AsFloat) != val->AsFloat)
		return FALSE;			/* Couldn't be forced */
	    val->AsInt = (ProLong)val->AsFloat;	/* Could be */
	    val->Float = FALSE;			/* note the change */
	}
	return TRUE;				/* is now an integer */
    }


/*  eval(t,g) traverses an expression tree, and puts its value and  type
    in  the "register" reg.  t is the term (dereferenced and unwrapped),
    and g is the associated global frame (if t  is  a  skeleton).   Once
    upon  a  time this used to return a structure explicitly, but some C
    compilers lack that facility, and this method is in fact faster  and
    less store-hungry.
*/
 void eval(t, frame)
    register PTR t;
    PTR frame;
    {
	register int typ;
	int op, fl;
	PTR argSkel, argEnv;
	Value rhs;
	FUNCTORP fn;

	if (IsPrim(t)) {			/* primitive type */
	    if (IsInt(t)) {			/* integer */
		reg.Float = FALSE,
		reg.AsInt = XtrInt(t);
		return;
	    }
	    if (IsFloat(t)) {			/* float */
		reg.Float = TRUE,
		reg.AsFloat = XtrFloat(t);
		return;
	    }
	    ArithError("dbrefs aren't numbers");
	}
	if (IsAtomic(t)) {			/* atom */
	    switch (AtomP(t)->flgsofae & 15) {
		case TIME:
		    reg.Float = TRUE, reg.AsFloat = CpuTime();
		    return;
		case HEAP:
		    reg.Float = FALSE, reg.AsInt = sizeof(PTR)*HeapUsed();
		    return;
		case STACK:
		    reg.Float = FALSE, reg.AsInt = sizeof(PTR)*(Regs_v1-glb0);
		    return;
		case PI:
		    reg.Float = TRUE, reg.AsFloat = 3.14159265358979;
		    return;
		case LOG2:
		    reg.Float = TRUE, reg.AsFloat = 0.693147180559945;
		    return;
		default:
		    NotOp(FunctorP(t));
	    }
	}
	if (IsRef(t) && IsUnbound(t))	/* undefined cell */
	    ArithError("unbound variable");

	fn = SkelFuncP(t);		/* compound (first check for [_]) */
	if (fn == listfunc) {
	    if (argv(SkelP(t)->Arg2, frame, &argEnv) != atomnil)
		ArithError("not a string of 1 char");
	    argSkel = argv(SkelP(t)->Arg1, frame, &argEnv);
	    eval(argSkel, argEnv);
	    return;
	}

	errno = 0;				/* no errors */
	op = fn->flgsoffe & 15;			/* grab operator number */
	switch (fn->arityoffe) {		/* dispatch on arity */
	    case 1:     			/* unary */
		argSkel = argv(SkelP(t)->Arg1, frame, &argEnv);
		eval(argSkel, argEnv);
		fl = fl_unary[op];
		typ = fl-1+AllFloat > 0;	/* force args to float? */
		if (fl == NEVER) {
		    if (!ForceInt(&reg)) NotInt(fn);
		} else
		if (reg.Float) {
		    typ = TRUE;
		} else
		if (typ) {
		    reg.Float = TRUE,
		    reg.AsFloat = (double)(reg.AsInt);
		}
		switch (op) {
		    case 0:			/* undefined unary operator */
			NotOp(fn);
		    case ID:
		    case FLOAT:
			return;
		    case NOT:
			reg.AsInt = ~reg.AsInt;
			return;
		    case UMINUS:
			if (typ) reg.AsFloat = -reg.AsFloat;
			else     reg.AsInt   = -reg.AsInt;
			break;
		    default:		/* ALWAYS functions */
			reg.AsFloat = (*UFloat[op])(reg.AsFloat);
			break;
		}
		if (errno != 0) ArithError(SysError());
		return;
	    case 2:				/* binary */
		argSkel = argv(SkelP(t)->Arg2, frame, &argEnv),
		eval(argSkel, argEnv);		/* rhs = arg 2 */
		CopyValue(rhs, reg);
		argSkel = argv(SkelP(t)->Arg1, frame, &argEnv),
		eval(argSkel, argEnv);		/* reg = arg 1 */
		fl = fl_binary[op];
		typ = fl-1+AllFloat > 0;	/* force args to float? */
		if (fl == NEVER) {
		    if (!ForceInt(&reg) || !ForceInt(&rhs)) NotInt(fn);
		} else {
		    typ |= reg.Float | rhs.Float;
		    if (typ) {			/* coerce both args to float */
			if (!reg.Float) reg.AsFloat = (double)(reg.AsInt);
			if (!rhs.Float) rhs.AsFloat = (double)(rhs.AsInt);
		    }
		    reg.Float = typ;
		}
		switch (op) {
		    default:
			NotOp(fn);
		    case PLUS:
			if (typ) reg.AsFloat += rhs.AsFloat;
			else     reg.AsInt   += rhs.AsInt;
			break;
		    case MINUS:
			if (typ) reg.AsFloat -= rhs.AsFloat;
			else     reg.AsInt   -= rhs.AsInt;
			break;
		    case TIMES:
			if (typ) reg.AsFloat *= rhs.AsFloat;
			else     reg.AsInt   *= rhs.AsInt;
			break;
		    case DIVIDE:
			if (typ) reg.AsFloat /= rhs.AsFloat;
			else     reg.AsInt   /= rhs.AsInt;
			break;
		    case MOD:
			reg.AsInt %= rhs.AsInt;
			return;
		    case AND:
			reg.AsInt &= rhs.AsInt;
			return;
		    case OR:
			reg.AsInt |= rhs.AsInt;
			return;
		    case LSHIFT:
			reg.AsInt <<= rhs.AsInt;
			return;
		    case RSHIFT:
			reg.AsInt >>= rhs.AsInt;
			return;
		    case IDIV:
			reg.AsInt /= rhs.AsInt;
			break;
		    case POW:
			reg.AsFloat = pow(reg.AsFloat, rhs.AsFloat);
			break;
		    case ATAN2:
			reg.AsFloat = atan2(reg.AsFloat, rhs.AsFloat);
			break;
		}
		if (errno) ArithError(SysError());
		return;
	    default:
		NotOp(fn);		/* arity > 2 */
	}
    }


ProLong intval(p)
    PTR p;
    /*	Evaluates an expression as an integer, and causes an event  */
    /*	if the resulting value is not an integer.  */
    {
	PTR e;

	p = vvalue(p, &e);
	eval(p, e);
	if (reg.Float && !Narrow(reg.AsFloat, &(reg.AsInt)))
	    ArithError("Integer expected");
	return reg.AsInt;
    }


PTR numeval(p)
    PTR p;
    /* Evaluates expression p and returns a number representation */
    {
	PTR e;

	p = vvalue(p, &e);
	eval(p, e);
	return reg.Float && !Narrow(reg.AsFloat, &(reg.AsInt))
	    ?  ConsFloat(reg.AsFloat) : ConsInt(reg.AsInt);
    }


int numcompare(op, t1, t2)
    int op;
    PTR t1, t2;
    /* Applies comparison operation op to expressions t1 and t2 */
    {
	PTR argSkel, argEnv;
	Value rhs;

	argSkel = vvalue(t2, &argEnv); eval(argSkel, argEnv);
	CopyValue(rhs, reg);
	argSkel = vvalue(t1, &argEnv); eval(argSkel, argEnv);
	if (reg.Float || rhs.Float) {
	    if (!reg.Float) reg.AsFloat = (double)(reg.AsInt);
	    if (!rhs.Float) rhs.AsFloat = (double)(rhs.AsInt);
	    switch (op) {
		case EQ:	return reg.AsFloat == rhs.AsFloat;
		case NE:	return reg.AsFloat != rhs.AsFloat;
		case LT:	return reg.AsFloat <  rhs.AsFloat;
		case GT:	return reg.AsFloat >  rhs.AsFloat;
		case LE:	return reg.AsFloat <= rhs.AsFloat;
		case GE:	return reg.AsFloat >= rhs.AsFloat;
		default:	return NEVER;
	    }
	}
	switch (op) {
	    case EQ:	return reg.AsInt == rhs.AsInt;
	    case NE:	return reg.AsInt != rhs.AsInt;
	    case LT:	return reg.AsInt <  rhs.AsInt;
	    case GT:	return reg.AsInt >  rhs.AsInt;
	    case LE:	return reg.AsInt <= rhs.AsInt;
	    case GE:	return reg.AsInt >= rhs.AsInt;
	    default:	return NEVER;
	} 
    }



/*----------------------------------------------------------------------+
|									|
|     These two functions are totally dependant  on  the  floating	|
|     point  number  representation  of  the  machine.   To  fit a	|
|     machine  float  into  a  constructed  float,  the  3   least	|
|     significant  bits  of  the mantissa are dropped.  In the VAX	|
|     F_floating format, the least significant  bits  are  on  the	|
|     right  in  the  second  16  bit  word of the value.  In IEEE	|
|     single precision floating point, the least significant  bits	|
|     are  on  the  right.   The  Perq  appears  to  use  the IEEE	|
|     representation.    The   Orion   also    uses    the    IEEE	|
|     representation.   This  is the only file which uses the IEEE	|
|     test, so just add your machine to the #if below.			|
|									|
+----------------------------------------------------------------------*/

typedef union Mixed {
	float	asfloat;
	PTR	asPTR;
    }   Mixed;


PTR ConsFloat(f)
    float f;
    {
	Mixed m;

	m.asfloat = f;
	return m.asPTR;
    }


float XtrFloat(p)
    PTR p;
    {
	Mixed m;

	m.asPTR = p;
	return m.asfloat;
    }



