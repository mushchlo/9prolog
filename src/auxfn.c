/************************************************************************
*									*
*                  C Prolog    auxfn.c					*
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

#include <u.h>
#include <libc.h>
#include <string.h>
#include "pl.h"

#if	COUNTING
	ProLong funcspace;
	int  funccount, atomcount;
#endif	COUNTING


/*  lookup(str) looks up a string in the hash table.  The hash table  is
    regrettably  small,  with a couple of thousand atoms in a moderately
    large program and only 256 hash chains, the chains get rather  long.
    The  hash  table  could be kept in the heap; if we did that we could
    make it bigger every so often.  Older versions of this routine  used
    to  have  a much larger critical region.  I have adopted the rule of
    thumb that a critical region only needs to contain  the  code  which
    links an object into a permanent data structure or out of one; as we
    now delay incrementing atomfp and linking the atom in until the last
    possible  moment, if we get interrupted elsewhere it just means that
    the atom is built but abandoned.  In other parts of code  this  rule
    means  that  we  can lose track of part of the heap, but that is not
    particularly important.  Note that  the  old  critical  region  code
    could  lose bits of the heap too.  The hash table size is now double
    the old size.  The hashing function is surprisingly good, and  would
    work for even larger tables.

    Fernando Pereira's versions 1.4a.sri and 1.5.sri have an extra chain
    in the hash table, and you can conceal an atom by moving it from its
    proper hash chain to "lostatoms".  I have decided not to implement a
    feature of this sort, as its absence has proven very useful to me as
    I debugged and developed the system, and it hasn't harmed any users.
*/

ATOMP lookup(id)
    char *id;
    {
	register ATOMP *ptr, atm;
	{
	    register char *s = id;
	    register int hash = 0;
	    while (*s) hash += *s++;
	    hash %= HashSize;
	    ptr = (ATOMP*)(hasha + hash);
	}
	while ((atm = *ptr) != AtomP(0)) {
	    if (!strcmp(atm->stofae, id)) return atm;
	    ptr = &(atm->nxtofae);
	}
	{
	    register ProLong size = Words(strlen(id)+szofae);

	    if (atomfp+size > atmax) NoSpace(AtomId);
	    atm = AtomP(atomfp);
	    atm->atofae = atm,
	    atm->arityofae = 0,
	    atm->flgsofae = 0,
	    atm->infxofae = atm->prfxofae = atm->psfxofae = 0,
	    atm->defsofae = atm->dbofae = NullC,
	    atm->fcofae = FunctorP(0),
	    atm->nxtofae = AtomP(0);
	    Ignore strcpy(atm->stofae, id);
	    Unsafe();
	    *ptr = atm;
	    atomfp += size;
	    Safe();
#if	COUNTING
	    atomcount++;
#endif	COUNTING
	    return atm;
	}
    }

/*  Given an atom and an arity, fentry either finds the functor block for
    that arity, or constructs one if there was none.  Note that arity==0
    is entirely possible, so the result could be an atom block.   The two
    are laid out as similarly as possible, so the caller never cares.
*/

FUNCTORP fentry(atom, arity)
    ATOMP atom;
    int arity;
    {
	register FUNCTORP old, new;

	for (old = FunctorP(atom); ; old = old->nxtoffe) {
	    if (old->arityoffe == arity) return old;
	    if (old->nxtoffe == FunctorP(0)) break;
	}
	new = FunctorP(getsp(szoffe+arity));
	new->atoffe = atom,
	new->arityoffe = arity,
	new->flgsoffe = 0,
	new->moreflgs = 0,
	new->nxtoffe = FunctorP(0),
	new->defsoffe = new->dboffe = NullC;
	{   register int i = 0;
	    register PTR *p = &(new->gtoffe);
	    *p = (PTR)new;	/* general term's functor is self */
	    while (i < arity) *++p = SkelGlobal(i++);
	}			/* new is now complete and can be added */
	Unsafe();
	old->nxtoffe = new;	/* put new in the chain */
	Safe();
#if	COUNTING
	funcspace += szoffe+arity, funccount++;
#endif	COUNTING
	return new;
    }


/*  Given an atom, an arity, and a vector of arguments, apply builds a
    Prolog representation of the desired term.   Note that it will NOT
    work for arity == 0, and is unnecessary in that case anyway.
*/

PTR apply(atom, arity, args)
    ATOMP atom;
    register int arity;
    register PTR *args;
    {
	PTR skeleton = Addr(fentry(atom, arity)->gtoffe);
	register PTR result = Regs_v1;

	while (--arity >= 0) *CellP(result++) = *args++;
	MolP(result)->Sk = skeleton;
	MolP(result)->Env = Regs_v1;
	if ((Regs_v1 = result+MolSz) > v1max) NoSpace(GlobalId);
	return result;
    }


/*  makelist(n, &elements) constructs a NON-EMPTY list.
    The counter n includes the final nil.
*/

PTR makelist(n, elements)
    register ProLong n;
    register PTR *elements;
    {
	register int i;
	register PTR *r, f;

	r = CellP(f = Regs_v1);
	if (f+n > v1max) NoSpace(GlobalId);
	for (elements += n; n > 10; n -= 9) {
	    for (i = 10; --i >= 0; *r++ = *--elements) ;
	    *elements++ = (PTR)r,	/* new cdr */
	    *r++ = list10, *r++ = f, f = (PTR)r;
	}
	for (i = n; --i >= 0; *r++ = *--elements) ;
	*r++ = list10 + (10-n)*3, *r++ = f, Regs_v1 = (PTR)r;
	return (PTR)r - 2;
    }


int list_to_string(list, s, n)
    register PTR list;
    register char *s;
    int n;
    {
	register PTR a;
	PTR env;		/* can't be a register because &env used */
	DeclRegisters		/* seems worthwhile because used in a loop */
	InitRegisters
    
	if (IsaRef(list) && !IsUnbound(list)) {
	    env = MolP(list)->Env, list = MolP(list)->Sk;
	    while (!IsaAtomic(list)) {
		/* list is dereffed, so if IsRef(list), SkelFuncP(list) == 0 */
		if (SkelFuncP(list) != listfunc || --n < 0) return FALSE; /* ERROR */
		a = arg(SkelP(list)->Arg1, env);
		list = argv(SkelP(list)->Arg2, env, &env);
		if (!IsByteInt(a)) return FALSE;
		*s++ = XtrByte(a);
	    }
	}
	if (list != atomnil) return FALSE;
	*s = '\0';
	return TRUE;
    }


/*  clause_number is given a stack frame.  That frame contains a pointer
    to the clause it is currently running.  Clause_number says which of
    the clauses in the definition of the predicate that clause is.  This
    number ranges from 1 up to the number of clauses in the predicate.
    It could be that there is something wrong with the frame or that the
    clause has been deleted.  If so, clause_number returns 0.  The goal
    could be obtained by looking in the frame, but as whenever we call
    clause_number we've already extracted the goal, so why bother?
*/

int clause_number(frame, goal)
    FRAMEP frame;
    PTR goal;
    {
	register CLAUSEP d, *n, *o;
	CLAUSEP *f = frame->altofcf;
	int pos;

	for (d = SkelFuncP(goal)->defsoffe, o = (CLAUSEP*)0, pos = 1;
	/*while*/ (n = &(d->altofcl)) != f;
	/*doing*/ o = n, d = *n) {
	    if (*n == NullC || n == o) return 0;
	    if (!(d->infofcl & ERASED)) pos++;
	}
	return pos;
    }


/*  backtrace() prints the stack.  Note that it changes the variable X (though
    it does restore it at the end).  This is so that pwrite will get the right
    local frame for the goals it prints, in addition to the right global frame
    (held in env).   This problem doesn't arise for other calls of pwrite() as
    either X is already right (which is the case in the debugger), or else the
    term is guaranteed not to contain any local variables (everywhere else).
*/
void backtrace()
    {
	register FRAMEP frame;
	FRAMEP saveX = Regs_X;
	FRAMEP prevframe;
	register PTR goal;
	PTR env;
	int depth = 0;
	int telling = Output;

	Output = STDOUT;
	for (frame = Regs_X, prevframe = FrameP(0);
	/*while*/ frame != prevframe;
	/*doing*/ prevframe = frame, frame = frame->gfofcf)
	    if (IsVisible(frame->infofcf)) {
		goal = frame->gofcf, Regs_X = frame->gfofcf;
		if (IsRef(goal)) {
		    env = MolP(goal)->Env, goal = MolP(goal)->Sk;
		} else {
		    env = Regs_X->gsofcf;
		}
		Ignore sprint(OutBuf, "%3d (%3ld) %2d: ",
		    --depth, (frame->infofcf>>LEVEL_WIDTH) & CALL_NUMBER,
		    clause_number(frame, goal));
		PutString(OutBuf);
		ProPWrite(goal, env, 1200);
		Put('\n');
	    }
	Output = telling, Regs_X = saveX;
    }


void Statistics()
    {
	int area;
	ProLong used;

//	char *sbrk();

	for (area = 0; area < NAreas; area++) {
	    switch (area) {
		case AtomId:	used = atomfp-atom0;	break;
		case AuxId:	used = vrz-auxstk0;	break;
		case TrailId:	used = Regs_tr-tr0;		break;
		case HeapId:	used = HeapUsed();	break;
		case GlobalId:	used = Regs_v1-glb0;		break;
		case LocalId:	used = Regs_v-lcl0;		break;
		default:	used = 0;
	    }
	    ProError("%-13s: %3ldK (%6ld bytes used)\n",
		AreaName[area], Size[area]/1024, used*sizeof(PTR));
	}
#if	COUNTING
	ProError("%d functor blocks occupy %ld words.\n", funccount, funcspace);
	ProError("%d atom blocks occupy %ld words.\n", funccount, atomfp-atom0);
#endif	COUNTING
//        ProError("unused       : %3dK\n", ((char *)(&used) - sbrk(0))/1024);
	ProError("Runtime      : %3.2fs.\n", CpuTime());
    }



