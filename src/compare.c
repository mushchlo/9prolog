/************************************************************************
*									*
*		   C Prolog	compare.c				*
*		   ========	---------				*
*									*
*  By Fernando Pereira, July 1982.					*
*  EdCAAD, Dept. of Architecture, University of Edinburgh.		*
*									*
*  Based on the Prolog system written in IMP by Luis Damas for ICL 2900	*
*  computers, with some contributions by Lawrence Byrd.  Stricter types	*
*  and several extensions by Richard O'Keefe, also BACKWARDS support.	*
*									*
*  Copyright (C) 1982 Fernando Pereira, Luis Damas and Lawrence Byrd.	*
*  Copyright (C) 1984 R.A.O'Keefe.					*
*									*
************************************************************************/

/*----------------------------------------------------------------------+
|									|
|   compare() always compares X->v1ofcf and X->v2ofcf.  It calls comp()	|
|   which actually compares the terms.  Comparison starts with a quick	|
|   filter, using "comparison codes".  The comparison code of a term is	|
|	if the term is a variable,	-3				|
|	if the term is a database ref,	-2				|
|	if the term is a number,	-1				|
|	if the term is an atom,		0				|
|	if the term is compound,	its arity (>= 1)		|
|   This manages to combine type comparison and arity comparison in a	|
|   single test.  The Dec-10 system did this first.  The really hairy	|
|   bit is that since the first word of an atom entry is a pointer to	|
|   itself, SkelFuncP(<atom>) = SkelAtomP(<atom>) = <atom>.  UGH.  I	|
|   have changed the original code four ways.  First, I have made a	|
|   distinction between data base references and numbers; intsign was	|
|   not defined on dbrefs.  Second, I have unfolded strcmp inline.	|
|   Third, I have made the last argument comparison a tail recursion,	|
|   similar to gunify().  This should make list comparison cheaper.	|
|   And fourth, I have unfolded intsign() in-line.  There is no other	|
|   call to it.  The order of dbrefs is arbitrary, but consistent.	|
|									|
+----------------------------------------------------------------------*/

#include "pl.h"

#define code(t)	(IsRef(t) ? -3 : !IsPrim(t) ? SkelFuncP(t)->arityoffe \
		:IsNumber(t) ? -1 : -2)

ProLong comp(T1,E1, T2,E2)
    register PTR T1, T2;
    PTR E1, E2;
    {
	register int n;		/* basically an arity */
start:
	/*  compare the codes of the two terms  */

	n = code(T1);
	{
	    register int d = n - code(T2);
	    if (d != 0) return d;
	}

	/*  they are the same sort of term, see if they are identical  */

	if (T1 == T2 && E1 == E2) return 0;

	/*  if they are not atoms or compound terms, compare the Ti  */

	if (n < 0) {
	    double d1, d2;

	    if (n < -1) return Signed(T1) - Signed(T2);
	    d1 = IsInt(T1) ? (double)XtrInt(T1) : XtrFloat(T1);
	    d2 = IsInt(T2) ? (double)XtrInt(T2) : XtrFloat(T2);
	    return d1 > d2 ? 1 : d1 < d2 ? -1 : 0;
	}

	/*  If they have the same principal functor, compare the arguments  */
	/*  They cannot be the same atom, or <T1,E1>==<T2,E2> would be true  */
	/*  If they have the same principal functor, the arities are equal  */
	/*  so compare the names of the atoms of the functors as strings.  */

	if (SkelFuncP(T1) == SkelFuncP(T2)) {
	    PTR t1,e1, t2,e2;
	    register int d;

	    if (n == 0) return 0;		/* an atom slipped by */
	    while (--n > 0) {
		t1 = argv(NextArg(T1), E1, &e1);
		t2 = argv(NextArg(T2), E2, &e2);
		d = comp(t1,e1, t2,e2);
		if (d != 0) return d;
	    }
	    T1 = argv(NextArg(T1), E1, &E1);	/* the last arguments are handled */
	    T2 = argv(NextArg(T2), E2, &E2);	/* as an explicit tail recursion  */
	    goto start;				/* to make list comparison faster */
	} else {				/* They have different functors */
	    register char *s1 = SkelAtomP(T1)->stofae;
	    register char *s2 = SkelAtomP(T2)->stofae;
	    while (*s1 == *s2) if (*s1++) s2++; else return 0;
	    return *s1 - *s2;
	}
    }


/*  icompare() knows that it is always being called to compare X->v1ofcf
    and X->v2ofcf, and that it is expected to return an integer <, =, or
    > 0 as appropriate.  Similarly, acompare() knows that  it  is  being
    called  to  compare X->v2ofcf and X->v3ofcf, and that it is expected
    to return an atom '<', '=', or '>'.  The reason for specialising the
    procedures this  way  is  to  eliminate  the  cost  of  passing  the
    arguments.  kcompare() is a specialised comparison routine for sort,
    msort, and keysort, to compare the nth argument of two terms.  If it
    finds either term too short, it returns NullP, otherwise < = or >.
*/

ProLong icompare()
    {
	PTR E1 = NullP;
	PTR T1 = vvalue(Addr(Regs_X->v1ofcf), &E1);
	PTR E2 = NullP;
	PTR T2 = vvalue(Addr(Regs_X->v2ofcf), &E2);
	return comp(T1,E1, T2,E2);
    }


ATOMP acompare()
    {
	PTR E1 = NullP;
	PTR T1 = vvalue(Addr(Regs_X->v2ofcf), &E1);
	PTR E2 = NullP;
	PTR T2 = vvalue(Addr(Regs_X->v3ofcf), &E2);
	ProLong n  = comp(T1,E1, T2,E2);
	return n < 0 ? LessThan : n > 0 ? GreaterThan : Equal;
    }


ATOMP kcompare(T1,E1, T2,E2, n)
    register PTR T1, T2;	/* pointers to skeletons */
    PTR E1, E2;			/* pointers to global frames */
    register int n;		/* argument number (> 0) */
    {
	if (SkelFuncP(T1)->arityoffe < n
	||  SkelFuncP(T2)->arityoffe < n) return AtomP(0);
	T1 = argv((PTR)(T1[n]), E1, &E1);
	T2 = argv((PTR)(T2[n]), E2, &E2);
	n  = comp(T1,E1, T2,E2);
	return n < 0 ? LessThan : n > 0 ? GreaterThan : Equal;
    }


