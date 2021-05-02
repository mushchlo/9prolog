/************************************************************************
*									*
*		   C Prolog	dbase.c					*
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

#include "pl.h"

/*  Variable table  */

#define	OnHead	1		/* variable appears in clause head */
#define	OnBody	2		/* variable appears in clause body */
#define	Global	4		/* variable must go in global stack*/

 PTR vname[MaxVar];	/* variable "name" (address) */
 PTR vmask[MaxVar];	/* bits describing variable */
 ProInt nvars;		/* number of variables seen */

/*  Context  */

 ProInt level;		/* depth of term nesting */
 ProInt onhead;		/* inside head? */
 ProInt toomany;		/* too many variables? */
 ProInt intgoal;		/* primitive as goal? */

 PTR fakeglb, fakelcl;	/* for globalise/instance */

#define	NewSkel(n)	SkelP(getsp(SkelSz(n)))

 void freeskel() /*forward*/;


/*  dispose(c), where c is the address of a clause or record, gets rid of the
    space occupied by the clause; freeskel will recover the space used by the
    skeletons, and update reference counts or clauses and records pointed to
    by those skeletons, possibly causing recursive calls to dispose.
*/
 void 
dispose (register CLAUSEP clause)
    {
	freeskel(clause->bdyofcl);
	freeskel(clause->hdofcl);
	release((PTR)clause, szofcl);
    }


/*  Release the space occupied by a skeleton.  It does no harm to call
    freeskel(NullP), as IsAtomic(NullP) happens to be true!
*/
 void 
freeskel (register PTR term)
    {
	if (IsDBRef(term)) {
	    register CLAUSEP clause = XtrDBRef(term);

	    if (--(clause->refcofcl) == 0	/* this was the last reference */
	    &&  (clause->infofcl & ERASED)	/* and it is to be erased */
	    && !(clause->infofcl & IN_USE)	/* and it isn't still in use */	
	    )   dispose(clause);
	    /*  When these conditions are true, the clause is no longer in a
		chain, because it has been removed by erase or hide.
	    */
	} else
	if (!IsAtomic(term) && !IsRef(term) && !IsAtomic(SkelFuncP(term))) {
	    /* term is a skeleton, and not a functor record */
	    int arity = SkelFuncP(term)->arityoffe;
	    register int n = arity;
	    register PTR *t = CellP(term);
	    
	    while (--n >= 0) freeskel(*++t);
	    release(term, arity+1);
	}
    }


/*  unchain(ref), where ref is a data base reference, removes the object
    (whether clause or record) pointed to by  p  from  its  backtracking
    chain.   Clauses  are  unchained as soon as they are erased, so that
    the interpreter main loop  won't  have  to  check  for  Erased(ref);
    records  are  unchained  only  when  their in-use flag is cleared on
    backtracking, so that new recordz-s will be seen.

    There is a difference between V1.2.D.edai and V1.2b.sri.  Since  the
    first  clause  in a list now points to the last one, a clause cannot
    have a null prevofcl if it is in a chain.  There is thus no need for
    a special UNCHAINED value, so I use null for that purpose.
*/

 void 
unchain (PTR ref)
    {
	register CLAUSEP clause;	/* the clause to erase */
	register CLAUSEP *hd;		/* the header of the clause chain */

	clause = XtrDBRef(ref);
	/* do nothing if the clause has already been unchained */
	if (clause->prevofcl == NullC) return;
	hd = IsClause(ref)
	   ? &(SkelFuncP(clause->hdofcl)->defsoffe)	/* CLAUSE */
	   : &(FunctorP( clause->hdofcl)->dboffe  );	/* RECORD */
	/* Exit early if this clause is hanging around after abolish/2 */
	if (*hd == NullC) {clause->prevofcl = NullC; return;}
	Unsafe();
	if (clause->altofcl == NullC) {	/* last element of chain */
	    (*hd)->prevofcl = clause->prevofcl;
	} else {			/* not last in chain */
	    clause->altofcl->prevofcl = clause->prevofcl;
	}
	if (clause == *hd) {		/* first element of chain */
	    *hd = clause->altofcl;
	} else {			/* not first in chain */
	    clause->prevofcl->altofcl = clause->altofcl;
	}
	clause->prevofcl = NullC;
	Safe();
    }


/*  hide(ref), where ref is a data base reference, is called when an
    ERASED object is left in in-use state on backtracking.  First, the
    IN_USE flag is reset.  At this point, we know that the object's
    backtracking chain is not being used.  If it is a clause, it will
    have been unchained before, but if it is a record it will have to
    be unchained if it hasn't been already.  In any case, unchain will
    check for us.  If the object is idle at this point, we can dispose
    of it.
*/
void 
hide (PTR ref)
    {
	register CLAUSEP clause = XtrDBRef(ref);

	clause->infofcl &= ~IN_USE;
	unchain(ref);
	if (clause->refcofcl == 0) dispose(clause);
    }


/*  Test whether a clause has been erased  */

int 
erased (PTR ref)
    {
	if (!IsDBRef(ref)) {
	    ErrorMess = "! erased: argument is not a reference";
	    return -1;
	}
	return XtrDBRef(ref)->infofcl & ERASED;
    }


/*  Erase the term pointed to by the reference ref  */

int 
erase (register PTR ref)
    {
	register CLAUSEP clause;	/* the clause to erase */

	if (!IsDBRef(ref)) {
	    ErrorMess = "! erase: argument is not a reference";
	    return FALSE;
	}
	clause = XtrDBRef(ref);
	if (clause->infofcl & ERASED) return TRUE;
	if (IsClause(ref)) {		/* CLAUSE */
	    if (SkelFuncP(clause->hdofcl)->flgsoffe & RESERVED) {
		ErrorMess = "! erase: argument is a system clause";
		return FALSE;
	    }
	    unchain(ref);		/* delink clauses at once */
	} else {			/* RECORD */
	    /* system has no protected records, delink records later */
	    if (!(clause->infofcl & IN_USE)) unchain(ref);
	}
	clause->infofcl |= ERASED;
	/* if the clause is idle, it will have been unchained by now, so
	   the chain will contain no junk if we dispose of it
	*/
	if (clause->refcofcl == 0 && !(clause->infofcl & IN_USE))
	    dispose(clause);
	return TRUE;
    }


/*  Copy a dynamic term, represented by a (Skel,Env) pair (t,frame) into
    the heap.   One of the effects is to rename the variables in the copy.
*/
 PTR 
termtoheap (register PTR t, PTR frame)
    {
	if (IsAtomic(t)) {
	    if (IsPrim(t) && level == 0) intgoal |= onhead ? OnHead : OnBody;
	    return t;
	}
	if (IsUnbound(t)) {		/* variable */
	    register int n;
	    register ProULong mask;

	    /*  look the variable up in the variable table  */

	    for (n = nvars; --n >= 0; )
		if (vname[n] == t) {
		    mask = ((ProULong)vmask[n]);
		    goto found_var;
		}			/* found repeated variable */
	    if (nvars == MaxVar) {
		toomany = TRUE;		/* too many variables */
		return ConsInt(0);	/* surplus turns into 0s */
	    }
	    n = nvars++, vname[n] = t, mask = 0;
found_var:
	    mask |= onhead ? OnHead : OnBody;
	    if (level > 1) {
	    	mask |= Global;
	    }
	    vmask[n] = (PTR)mask;
	    if (level == 0 && !onhead) {	/*  X -> call(X) in body */
	    	register SKELP s = NewSkel(1);
	    	s->Fn = calltag, s->Arg1 = SkelGlobal(n);
	    	return (PTR)s;
	    }
	    return SkelGlobal(n);
	}
	if (IsRef(t)) frame = MolP(t)->Env, t = MolP(t)->Sk;
	{
		register int n = SkelFuncP(t)->arityoffe;
	    SKELP s = NewSkel(n);
	    register PTR *p = CellP(s);
	    PTR argSkel, argEnv;
	    level++;
	    *p = (PTR)SkelFuncP(t);
	    while (--n >= 0) {
		argSkel = argv(NextArg(t), frame, &argEnv);
		*++p = termtoheap(argSkel, argEnv);
	    }
	    level--;
		return (PTR)s;
	}
}


/*  Copy the body of a clause onto the heap  */

 PTR 
bodytoheap (register PTR t, PTR frame)
    {
	if (IsAtomic(t) || IsUnbound(t)) return termtoheap(t, frame);
	if (IsRef(t)) frame = MolP(t)->Env, t = MolP(t)->Sk;
	if (SkelFuncP(t) == commatag) {
	    register PTR argSkel;
	    PTR argEnv;
	    register SKELP s = NewSkel(2);
	    
	    s->Fn = commatag;
	    argSkel = argv(SkelP(t)->Arg1, frame, &argEnv);
	    s->Arg1 = termtoheap(argSkel, argEnv);
	    argSkel = argv(SkelP(t)->Arg2, frame, &argEnv);
	    s->Arg2 = bodytoheap(argSkel, argEnv);
	    return (PTR)s;
	}
	return termtoheap(t, frame);
    }


/*  Scan a clause or record in the heap, converting the  format  of  its
    variables.    We  also  increment  the  reference  counts  of  other
    clauses/records now, because only now are we sure that the clause is
    really in the database.  This eliminates yet another bug!
*/

 void 
scan (PTR *c)
    {
	register PTR t = *c;

	if (IsAtomic(t)) {
	    if (IsDBRef(t)) XtrDBRef(t)->refcofcl++;
	} else
	if (IsVar(t)) {
	    *c = vmask[t-SkelGlobal(0)];
	} else {
	    register int n = SkelFuncP(t)->arityoffe;
	    while (--n >= 0) scan(CellP(++t));
	}
    }


/*  wipe out all the clauses (NB not the facts stored  under  this  key)
    for  a particular predicate.  This should be done once per predicate
    per reconsult.  The table of predicates which have  been  loaded  in
    this  call  of $csult is kept in [vra..vrz), which is a very strange
    name, but there it is.  The UserCall argument is true when  this  is
    called  from  the  predicate  abolish/2, and false when it is a call
    from record() implementing $assertr/1.   If we are interrupted while
    this is going on,  the predicate will be in the table of abolitions,
    but only some of the clauses will have gone.   Since the structures,
    though wrong, will be well-formed, I find this acceptable. 'unchain'
    has the necessary code in a critical region, so this has none.
*/
void 
abolish (register FUNCTORP fn, int UserCall)
    {
	if (!UserCall) {
	    register PTR seen = vra;
	    while (seen < vrz) if (FunctorP(*seen++) == fn) return;
	    if (seen >= auxmax) NoSpace(AuxId);
	    *(FUNCTOR**)vrz++ = fn;
	}
	{
	    register CLAUSEP clause, next;

	    for (clause = fn->defsoffe; clause != NullC; clause = next) {
		next = clause->altofcl;
		clause->infofcl |= ERASED;
		if (!(clause->infofcl & IN_USE)) {
		    unchain(ConsDBRef(clause, CLAUSE));
		    if (clause->refcofcl == 0) dispose(clause);
		}
	    }
	}
	fn->defsoffe = NullC;
    }


/*  record a term (t) in the data base, under the key (rk) if
    (key) = RECORD, simply asserting it if (key) = CLAUSE and
    (rk) = FALSE, or doing special 'reconsult' erasure first
    if (key) = CLAUSE and (rk) = TRUE.  The new record is to
    go at the beginning (aorz) = TRUE or end (aorz) = FALSE 
    of the appropriate chain.  Intermediate positions are out.
*/
PTR 
record (ProLong key, PTR t, PTR rk, ProInt aorz)
{
	PTR head, body, g;
	register FUNCTOR *fn;
	ProInt ng = 0;	/* number of Global variables */
	ProInt nl = 0;	/* number of Local variables */
	ProInt nt = 0;	/* number of Temporaries */

	nvars = 0, toomany = FALSE, intgoal = 0;
	if (IsRef(t) && !IsUnbound(t)) g = MolP(t)->Env, t = MolP(t)->Sk;
	
	if (key == RECORD) {		/* adding a record to the data base */
	    register PTR rkey = rk;
	    if (IsRef(rkey)) {
	    	rkey = VarVal(rkey);
	    	if (!IsAtomic(rkey)) rkey = (PTR)SkelFuncP(rkey);
	    }
	    if (IsPrim(rkey) || IsRef(rkey)) {
	    	ErrorMess = "! Illegal database key";
			return NullP;
	    }
	    level = 1, onhead = FALSE;
	    body = termtoheap(t, g);
	    if (toomany) {		/* variable overflow */
	    	ErrorMess = "! Too many variables in record";
	    	freeskel(body);
	    	return NullP;
	    }
	    fn = FunctorP(head = rkey);
	} else {			/* adding a clause to the rule base */	
	    level = 0, onhead = TRUE;
	    if (IsAtomic(t) || SkelFuncP(t) != arrowtag) {
	    	head = termtoheap(t, g);
	    	body = NullP;
	    } else {
	    	PTR a, f;
	    	a = argv(SkelP(t)->Arg1, g, &f), head = termtoheap(a, f);
	    	onhead = FALSE;
	    	a = argv(SkelP(t)->Arg2, g, &f), body = bodytoheap(a, f);
	    }
	    if (IsVar(head)) {
	    	ErrorMess = "! Clause head is a variable";
			goto errorexit;
	    }
	    if ((intgoal & OnHead)) {
	    	ErrorMess = "! Clause head is a number";
	    	goto errorexit;
	    }
	    if ((intgoal & OnBody) && !InBoot) {
	    	ErrorMess = "! Clause body calls a number";
	    	goto errorexit;
	    }
	    if (toomany) {
	    	ErrorMess = "! Too many variables in clause";
			goto errorexit;
	    }
	    fn = SkelFuncP(head);
	    if (fn->flgsoffe & RESERVED) {
	    	ErrorMess = "! Attempt to redefine a system predicate";
errorexit:	freeskel(head);
			freeskel(body);
			return NullP;
	    }
	    if ((ProLong)rk != FALSE) abolish(fn, FALSE);	/* reconsult */
	}
	{   /* convert variable numbers to variable patterns */
	    /* note that the order in which vmask is scanned MATTERS */
	    register ProInt j;
	    register ProInt k;

	    for (j = 0; j < nvars; j++)
	    	vmask[j] =
	    			Unsigned(vmask[j]) & Global ? SkelGlobal(ng++)
	    			: Unsigned(vmask[j]) & OnBody ? SkelLocal(nl++)
	    			: (PTR)(nt++);
	    if (nt > 0)
		for (j = 0; j < nvars; j++)
		    if ((k = Signed(vmask[j])) < nt)
		    	vmask[j] = SkelLocal(k+nl);
	    if (body != NullP) scan(&body);
	    if (key == CLAUSE) scan(&head);
	}
	{
	    register CLAUSEP *hd = key==CLAUSE?&(fn->defsoffe):&(fn->dboffe);
	    register CLAUSEP top = *hd; /* first element of chain */
	    register CLAUSEP clause = ClauseP(getsp(szofcl));

	    clause->hdofcl   = head,	/* head of clause */
	    clause->bdyofcl  = body,	/* body of clause */
	    clause->refcofcl = 0,	/* reference count, 0 is right! */
	    clause->infofcl  = 0,	/* flags (not erased or in-use) */
	    clause->ltofcl   = nl+nt,	/* #locals + temporaries */
	    clause->lvofcl   = nl,	/* #local variables excluding temps */
	    clause->gvofcl   = ng;	/* #global variables */

	    if (top == NullC && Announce)
		ProError("+ %s /%d\n",
		    fn->atoffe->stofae, fn->arityoffe);
	    Unsafe();
	    if (top == NullC) {		/* the chain was empty */
	    	clause->altofcl = NullC, clause->prevofcl = clause;
			*hd = clause;
	    } else
	    	if (aorz) {			/* store before first element (A) */
	    		clause->altofcl = top, clause->prevofcl = top->prevofcl, top->prevofcl = clause, *hd = clause;
	    	} else {			/* store after last element (Z) */
	    		clause->altofcl = NullC, clause->prevofcl = top->prevofcl, top->prevofcl->altofcl = clause, top->prevofcl = clause;
	    	}
	    Safe();
	    // -- cprolog.com -- debug
		//if(ProCheckClauseTry(clause, "$directive")) {
		//	ProShowClause(clause, nil, nil, 'A');
		//}
	    // -- cprolog.com -- debug
	    return ConsDBRef(clause, key);
	}
}


/*  Scan the recorded data base (implements recorded/1).
    key should be RECORD for the data base or CLAUSE for clauses.
    It must be long rather than int because RECORD and CLAUSE are long.
    The local stack is expected to contain
	X->v1ofcf    term to be matched
	X->v2ofcf    var to be unified with reference into data base
	X->v3ofcf    (is the key, but this procedure does not care)
	X->v4ofcf    indirect pointer to next term to be tried for match
    Note that this and the remaining functions in this file use the
    global variables X, v1, and tr.  They are only called in evalp.c.
*/

PTR 
recorded (ProLong key)
    {
	register FRAMEP rX = Regs_X;
	register CLAUSEP clause = ClauseP(*(rX->v4ofcf));
	PTR ov1 = Regs_v1, otr = Regs_tr;
	register PTR t;
	ProInt found;

	if (IsAtomic(clause)) return NullP;	/* undefined & system preds */
	for (; clause != NullC; clause = clause->altofcl) {
	    if (clause->infofcl & ERASED) continue;
	    if (key == CLAUSE) {
		found = TRUE;
	    } else {/* RECORD */
		InitGlobal(clause->gvofcl, t);
		found = unifyarg(Addr(rX->v1ofcf), clause->bdyofcl, ov1);
	    }
	    t = ConsDBRef(clause, key);
	    if (found && unifyarg(Addr(rX->v2ofcf), t, NullP)) {
		GrowLocal(4);	/* the variables were temporary */
		rX->v4ofcf = Addr(clause->altofcl);
		if (!(clause->infofcl & IN_USE)) {
		    clause->infofcl |= IN_USE;
		    TrailPtr(t);
		}
		return t;
	    }
	    for (t = Regs_tr; t != otr; VarVal(*--t) = NullP) ;
	    Regs_tr = t, Regs_v1 = ov1;	/* reset variables, free global stack */
	}
	return NullP;
    }


 PTR 
globalise (register PTR t, ProInt bodyflg)
    {
	if (IsAtomic(t)) {
	    return t;
	} else
	if (bodyflg && SkelFuncP(t) == commatag) {
	    register PTR a = Regs_v1;
	    GrowGlobal(2+2);
	    MolP(a)->Sk = Addr(commatag->gtoffe),
	    MolP(a)->Env = a+2,
	    *CellP(a+2) = globalise(SkelP(t)->Arg1, FALSE),
	    *CellP(a+3) = globalise(SkelP(t)->Arg2, TRUE);
	    return a;
	} else {
	    register PTR a = Regs_v1, b, arg;
	    register int n = SkelFuncP(t)->arityoffe;

	    MolP(a)->Sk = Addr(SkelFuncP(t)->gtoffe),
	    MolP(a)->Env = a+2,
	    b = a+1,
	    a += n+2;			/* a acts like v1 here */
	    while (--n >= 0) {
		arg = NextArg(t);
		if (!IsAtomic(arg)) {
		    if (IsVar(arg)) {
			arg = FrameVar(arg, fakeglb, fakelcl);
		    } else {		/* IsSkel(arg) */
			MolP(a)->Sk = arg, MolP(a)->Env = fakeglb;
			arg = a, a += MolSz;
		    }
		}
		NextArg(b) = arg;
	    }
	    if (a >= v1max) NoSpace(GlobalId);
	    b = Regs_v1, Regs_v1 = a;
	    return b;
	}
    }


/*  Given a data base reference "ref", construct a copy of the record or
    clause it points to (this is tricky for clauses as they may contain
    local variables) and bind that to the variable argp points to.  This
    implements the Prolog predicate instance/2.
*/
int 
instance (PTR ref, PTR argp)
    {
	register CLAUSEP cl;

	if (!IsDBRef(ref)) return -1;

	cl = XtrDBRef(ref);
	if (!IsClause(ref)) {		/* RECORD */
	    register PTR frame;

	    InitGlobal(cl->gvofcl, frame);
	    return unifyarg(argp, cl->bdyofcl, frame);

	} else {			/* CLAUSE */
	    register PTR head, body;
	    ProInt glovars = cl->gvofcl;
	    ProInt locvars = cl->ltofcl;

	    InitGlobal(glovars+locvars, fakeglb);
	    fakelcl = fakeglb + (glovars-szofcf);
	    head = cl->hdofcl, body = cl->bdyofcl;
	    if (body == NullP) body = atomtrue;
	    if (locvars > 0 ) {
		head = globalise(head, FALSE);
		body = globalise(body, TRUE);
	    } else {
		if (!IsAtomic(head)) ConsMol(head, fakeglb, head);
		if (!IsAtomic(body)) ConsMol(body, fakeglb, body);
	    }
	    *CellP(Regs_v1) = head, *CellP(Regs_v1+1) = body;
	    head = Addr(arrowtag->gtoffe), body = Regs_v1;
	    GrowGlobal(2);
	    return unifyarg(argp, head, body);
	}
    }


