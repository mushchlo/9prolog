/************************************************************************
*									*
*                  C Prolog    unify.c					*
*                  ========    -------					*
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

#include "pl.h"

/*  Unify term ta (with global frame ga and local frame V)
    with  term tb (with global frame gb and local frame X)
    The result is TRUE if the  terms  can  be  unified,  FALSE  if  they
    cannot.   In  the latter case, some substitutions may have been made
    and trailed which will need undoing.  By the time we get  here,  the
    terms  will  not  contain  any  local  variables,  thanks to the way
    locality is defined.  We are  guaranteed  that  the  two  terms  are
    compound.

    In order to make unification as fast as possible, the type tests all
    use the special registers.  Also, I have made  a  slight  change  to
    ConsMol.   ConsaMol(<term>,<env>,<temp>,<var>) is basically the same
    as ConsMol(<term>,<env>,VarVal(<var>)), but uses <temp> as a working
    register, which should make things a little faster.  Note that pa,pb
    are always dead when gunify() is called.

    The original code used the test IsVar(a) to check for variable codes
    in skeletons, but some of it exploited the fact that skeletons can't
    contain real pointers into the stacks, and used IsRef(a) instead.  I
    have made this explicit by defining IsaVar(x), which avoids the slow
    masking operation that was needed.  In fact, because comparisons are
    signed, and because stack addresses cannot appear in skeletons, we'd
    no need to do the masking at all.
*/

int gunify(ta, ga, tb, gb)
    PTR ta, ga, tb, gb;
{
	int arity;
	register PTR a, b, pa, pb;
	DeclRegisters
	InitRegisters

    /*  check that the terms have the same principal functor  */
start: 
	a = (PTR)SkelFuncP(ta);
	if ((PTR)SkelFuncP(tb) != a) return FALSE;
	arity = FunctorP(a)->arityoffe;

    /*  main loop  */

	while (--arity > 0) {
	    b = NextArg(tb);
	    a = NextArg(ta);
	    if (IsaVar(a)) {
		pa = FrameGlo(a, ga);
		while (IsaRef(a = VarVal(pa))) pa = a;
		if (IsaVar(b)) {
		    pb = FrameGlo(b, gb);
		    while (IsaRef(b = VarVal(pb))) pb = b;
		    if (pa == pb) {
		    } else
		    	if (Undef(b)) {
		    		if (!Undef(a)) {
		    			VarVal(pb) = IsaAtomic(a) ? a : pa;
		    			TrailGlo(pb);
		    		} else
		    			if (pa > pb) {
		    				VarVal(pa) = pb;
		    				TrailGlo(pa);
		    			} else {
		    				VarVal(pb) = pa;
		    				TrailGlo(pb);
		    			}
		    	} else
		    		if (Undef(a)) {
		    			VarVal(pa) = IsaAtomic(b) ? b : pb;
		    			TrailGlo(pa);
		    		} else
		    			if (IsaAtomic(a)) {
		    				if (a != b) return FALSE;
		    			} else
		    				if (IsaAtomic(b) || !gunify(a, MolP(pa)->Env, b, MolP(pb)->Env)) {
		    					return FALSE;
		    				}
		} else
			if (Undef(a)) {
				if (IsaAtomic(b)) VarVal(pa) = b;
				else ConsaMol(b, gb, pb, pa);
				TrailGlo(pa);
			} else
				if (IsaAtomic(b)) {
					if (a != b) return FALSE;
				} else
					if (IsaAtomic(a) || !gunify(a, MolP(pa)->Env, b, gb)) {
						return FALSE;
					}
    	} else
    		if (IsaVar(b)) {
    			pb = FrameGlo(b, gb);
    			while (IsaRef(b = VarVal(pb))) pb = b;
    			if (Undef(b)) {
    				if (IsaAtomic(a)) VarVal(pb) = a;
    				else ConsaMol(a, ga, pa, pb);
    				TrailGlo(pb);
    			} else
    				if (IsaAtomic(b)) {
    					if (b != a) return FALSE;
    				} else
    					if (IsaAtomic(a) || !gunify(a, ga, b, MolP(pb)->Env)) {
    						return FALSE;
    					}
    		} else {
    			if (IsaAtomic(a)) {
    				if (b != a) return FALSE;
    			} else
    				if (IsaAtomic(b) || !gunify(a, ga, b, gb)) {
    					return FALSE;
    				}
    		}
	}
	b = NextArg(tb);		/* tail recursion on last arguments */
	a = NextArg(ta);
	if (IsaVar(a)) {
		pa = FrameGlo(a, ga);
		while (IsaRef(a = VarVal(pa))) pa = a;
		if (IsaVar(b)) {
			pb = FrameGlo(b, gb);
			while (IsaRef(b = VarVal(pb))) pb = b;
			if (pa == pb) return TRUE;
			if (Undef(b)) {
				if (!Undef(a)) {
					VarVal(pb) = IsaAtomic(a) ? a : pa;
					TrailGlo(pb);
				} else
					if (pa > pb) {
						VarVal(pa) = pb;
						TrailGlo(pa);
					} else {
						VarVal(pb) = pa;
						TrailGlo(pb);
					}
				return TRUE;
			}
			if (Undef(a)) {
				VarVal(pa) = IsaAtomic(b) ? b : pb;
				TrailGlo(pa);
				return TRUE;
			}
			if (IsaAtomic(a)) return a == b;
			if (IsaAtomic(b)) return FALSE;
			ta = a, ga = MolP(pa)->Env,	tb = b, gb = MolP(pb)->Env;
			goto start;
		}
		if (Undef(a)) {
			if (IsaAtomic(b)) VarVal(pa) = b;
			else ConsaMol(b, gb, pb, pa);
			TrailGlo(pa);
			return TRUE;
		}
		if (IsaAtomic(a)) return a == b;
		if (IsaAtomic(b)) return FALSE;
		ta = a, ga = MolP(pa)->Env, tb = b;
		goto start;
	} else
		if (IsaVar(b)) {
			pb = FrameGlo(b, gb);
			while (IsaRef(b = VarVal(pb))) pb = b;
			if (Undef(b)) {
				if (IsaAtomic(a)) VarVal(pb) = a;
				else ConsaMol(a, ga, pa, pb);
				TrailGlo(pb);
				return TRUE;
			}
			if (IsaAtomic(a)) return a == b;
			if (IsaAtomic(b)) return FALSE;
			tb = b, gb = MolP(pb)->Env, ta = a;
			goto start;
		} else {
			if (IsaAtomic(a)) return a == b;
			if (IsaAtomic(b)) return FALSE;
			ta = a, tb = b;
			goto start;
		}
}


/*  Unify a variable, pointed to by  the  address  <arg>,  with  a  term
    <term>  having  global frame <frame>.  When this is called, <arg> is
    always Addr(X->v?ofcf), ?  being a digit 1..5,  though  it  may  not
    have  this  surface form.  Variables in the term will be global, but
    the argument itself is a local variable (which may  be  bound  to  a
    global).  The term is usually atomic.
*/

int unifyarg(arg, term, frame)
    register PTR arg, term;
    PTR frame;
    {
	register PTR argval;
	DeclRegisters
	InitRegisters

	while (IsaRef(argval = VarVal(arg))) arg = argval;
	if (Undef(argval)) {
	    if (IsaAtomic(term)) {
		VarVal(arg) = term;
	    } else
	    if (!IsaRef(term)) {	/* term is a skeleton */
		ConsaMol(term, frame, argval, arg);
	    } else
	    if (IsUnbound(term) && term >= arg) {
		if (term == arg) return TRUE;
		VarVal(term) = arg;
		arg = term;
	    } else {			/* term is a ref */
		VarVal(arg) = term;
	    }
	    TrailReg(arg);
	    return TRUE;
	}
	if (IsaRef(term) && IsUnbound(term)) {
	    VarVal(term) = IsaAtomic(argval) ? argval : arg;
	    TrailReg(term);
	    return TRUE;
	} else
	if (IsaAtomic(term)) {
	    return term == argval;
	} else
	if (!IsaRef(arg)) {
	    return FALSE;
	} else {
	    if (IsaRef(term)) frame = MolP(term)->Env, term = MolP(term)->Sk;
	    return gunify(MolP(arg)->Sk, MolP(arg)->Env, term, frame);
	}
    }


PTR vvalue(var, framev)
    register PTR var;
    PTR *framev;
    {
	register PTR dereffed;
	while (IsRef(dereffed = VarVal(var))) var = dereffed;
	if (Undef(dereffed)) return var;
	if (!IsAtomic(dereffed)) *framev = MolP(var)->Env;
	return dereffed;
    }


/*  arg(argp, frame) and argv(argp, frame, rframe)
    THIS COMMENT NEEDS REWRITING
    are both given a frame and a pointer to an argument of  a  skeleton.
    That  argument might be a pointer to another skeleton, an atom, or a
    variable code.  Both of these procedures return a representation  of
    the argument: arg returns a single pointer, which has the same frame
    if  one  is  needed,  and  argv  returns both a pointer, and, in the
    rframe argument, a new frame.  The only real difference is that argv
    will take apart a molecule and arg will not.  The old code for arg()
    used to return a skeleton if it was given a pointer to one.  When it
    if given a skeleton, it now constructs a molecule, as every call was
    either doing this already or didn't care.
*/

PTR arg(a, frame)
    register PTR a;
    PTR frame;
    {
	register PTR p;

	if (IsVar(a)) {			/* VARIABLE */
	    p = FrameVar(a, frame, Regs_x);
	    while (IsRef(a = VarVal(p))) p = a;
	    return IsAtomic(a) && !Undef(a) ? a : p;
	} else
	if (IsAtomic(a)) {		/* ATOM */
	    return a;
	} else {			/* SKELETON */
	    p = Regs_v1, Regs_v1 += MolSz,
	    MolP(p)->Sk = a, MolP(p)->Env = frame;
	    return p;
	}
    }


PTR argv(a, frame, af)
    register PTR a;
    PTR frame, *af;
    {
	register PTR p;

	if (!IsVar(a)) {	/* constant or pointer to skeleton */
	    *af = frame;	/* not needed for constant, but no matter */
	    return a;
	}			/* must be a local or global variable */
	p = FrameVar(a, frame, Regs_x);
	while (IsRef(a = VarVal(p))) p = a;
	if (IsAtomic(a)) {	/* constant or not bound */
	    *af = NullP;
	    return Undef(a) ? p : a;
	} else {		/* bound to a compound term */
	    *af = MolP(p)->Env;
	    return /*MolP(p)->Sk*/ a;
	}
    }


