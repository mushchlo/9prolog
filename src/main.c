/************************************************************************
*									*
*		   C Prolog      main.c					*
*		   ========	 ------					*
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
#include "evalp.h"
#include "arith.h"

#define TEST(c,s,f)	{if (c) goto s; else goto f;}
#define TRY(c)		TEST(c,EXIT,FAIL)

/* Variables for communication with read, write and symbol table */

int	 MouseX, MouseY, MouseButtons;

ATOMP	 BasicAtom[rqrdatoms];
FUNCTORP BasicFunctor[rqrdfuncs];
PTR	 hasha, atomfp, atprompt, list10;
int	 lc, quoteia, fileerrors, reading;

/*  execution and start-up flags */

int	running = FALSE;
int	PrologEvent;		/* why did we bomb out? */
extern	jmp_buf ZeroState;	/* where to bomb out to */

/* variables for communication with dbase */

PTR vra, vrz;

/*  general error message passing string */

char *ErrorMess;

/*  Heap management block */

extern	char Heap[];		/* these are just used to save() and */
extern	ProLong HeapHeader;	/* restore() the heap, not to look inside */
extern	PTR  HeapTop();

/* Prolog machine registers */

FRAMEP	Regs_X;		/* local frame pointer for parent goal */
FRAMEP	Regs_V;		/* local frame pointer for current goal */
FRAMEP	Regs_VV;		/* local frame pointer for last choice point */
PTR	Regs_x1;		/* global frame pointer for parent goal */
PTR	Regs_vv1;		/* global frame pointer for last choice point */
PTR	Regs_v1;		/* top of global stack */
PTR	Regs_tr;		/* top of trail */

/*  main loop variables */

 int usermode;	/* are we executing a user or a system predicate? */
 int lev;		/* depth of procedure calls */
 int invokno;	/* call number (NOT depth) */
 ProLong info;	/* package of invokno and lev */
 PTR pg;		/* the current goal (atom or molecule) */
 PTR g;		/* skeleton of the current goal */
 PTR c;		/* continuation (points to a skeleton) */
 CLAUSEP d;	/* current clause */
 CLAUSEP *fl;	/* ^list of alternatives */

#define FailToSelf {register CLAUSEP cl = *fl; fl = &(cl->altofcl); *fl = cl;}

/*  miscellaneous */

 int carith = 0;	/* are arithmetic goals to be "compiled"? */
 int bb;		/* flags of current predicate */

#if	COUNTING	/* recording stack excursion */
#define	NEXT_PORT	4
	double	totloc, totglo, tottr;
	ProLong	maxloc, maxglo, maxtr;
	ProLong	smpcnt, portct[5], instrct[NPREDS];
	FILE	*trace_file;
#endif	COUNTING

/* Variables and constants for the basic debugging package */

enum { CALL_PORT, EXIT_PORT, BACK_PORT, FAIL_PORT };
#define NEVER_SKIP	1000000

	char *portname[] = {"call", "exit", "back", "fail"};
	int   portmask[] = {8,	    4,	    2,	    1     };

	int debug = FALSE;	/* are we debugging (TRUE/FALSE) ? */
	int sklev;		/* skipped level to return to (set by ^C too) */
	int brklev = 0;		/* how many break levels we're inside */
	int unknown = 0;	/* fail/error when a predicate has no clauses? */
	int spy = FALSE;	/* FALSE for not spying, SPY_ME when spying */
	int port;		/* which port is 'message' to return to? */
	int leash = 10;		/* which ports is 'message' to stop at? */
	int dotrace;		/* trace trigger (checked at CALL port) */

	int  brtn;		/* where is 'break' to return to? */
	int  recons = FALSE;	/* FALSE -> consulting, TRUE -> reconsulting */
	PTR  brkp = NullP;	/* points to topmost break state */
	PTR *savead;		/* used in saving/restoring break states */

/*----------------------------------------------------------------------+
|									|
|		Entering and leaving breaks,				|
|		saving and restoring memory images.			|
|									|
+----------------------------------------------------------------------*/

/*  The variables to be saved  during  a  break  are  divided  into  two
    groups:  the  integers  (which  are  stored in Prolog coded form and
    converted back when the break is exited) and the pointers (which are
    stored as is).  This relies on  all  the  pointers  having  sensible
    Prolog coded values.  Because integers are coded, they can only have
    29-bit values.  Although the `info` variable is declared  `int`,  it
    is  actually  a  tagged  value.   The  break  level  brklev  is  now
    maintained by save/rest vars.  Note that a file whose index is  held
    in a break state cannot be closed.
*/

 int *BrkInts[] =
    {
	&bb, &brklev, &brtn, &debug, &dotrace, &invokno, &lc, &lev,
	&port, &recons, &sklev, &spy, &Input, &Output, &usermode, (int*)0
    };

 PTR *BrkPtrs[] =
    {
	&brkp, &c, &g, &pg, &vra, &vrz,
	(PTR*)&Regs_X, &Regs_x1, (PTR*)&Regs_V, &Regs_v1, (PTR*)&Regs_VV, &Regs_vv1, &Regs_tr,
	(PTR*)&d, (PTR*)&fl, (PTR*)&info, CellP(0)
    };


#if	debugging
 void DumpVars(message)
    char *message;
{
	ProError("bb\t%d\tbrklev\t%d\tbrtn\t%d\tdebug\t%d\n", bb, brklev, brtn, debug);
	ProError("dotrace\t%d\tinvokno\t%d\tlc\t%d\tlev\t%d\n", dotrace, invokno, lc, lev);
	ProError("port\t%d\trecons\t%d\tsklev\t%d\tspy\t%d\n", port, recons, sklev, spy);
	ProError("Input\t%d\tOutput\t%d\tbrkp\t%x\tc\t%x\n", Input, Output, brkp, c);
	ProError("g\t%x\tpg\t%x\tvra\t%x\tvrz\t%x\n", g, pg, vra, vrz);
	ProError("x\t%x\tx1\t%x\tv\t%x\tv1\t%x\n", X, x1, V, v1);
	ProError("vv\t%x\tvv1\t%x\ttr\t%x\n", VV, vv1, tr);
	ProError("\td\t%x\tfl\t%x\tusermode\t%d\n", d, fl, usermode);
	ProError("info\t%x\tsavead\t%x\t%s\n", info, savead, message);
}
#else
#define DumpVars(x)
#endif	debugging

void savev(p, n)
    register PTR *p;		/* starting at p */
    register int  n;		/* save n vars   */
{
	while (--n >= 0) *savead++ = *p++;
}

void restv(p, n)
    register PTR *p;		/* starting at p */
    register int n;		/* restore n vars */
{
	while (--n >= 0) *p++ = *savead++;
}


void savevars()		/*  to enter a break */
{
	PTR nbrkp;
	register int **pi;
	register PTR **pp;

	savead = CellP(nbrkp = Regs_v+MaxFrame);
	for (pi = BrkInts; *pi; *savead++ = ConsInt(*(*pi++))) ;
	for (pp = BrkPtrs; *pp; *savead++ = *(*pp++)) ;
	DumpVars("Before Save");
	vra = vrz, Regs_V = FrameP(savead), brkp = nbrkp, brklev++;
	LockChannels(TRUE);	/*  make Input, Output unclosable  */
	Input = STDIN, Output = STDOUT;
	DumpVars("After Save");
}

void restvars()	/*  to continue from a break */
{
	register int **pi;
	register PTR **pp;
	register PTR dum;	/* for XtrInt !!! */

	DumpVars("Before Restore");
	savead = CellP(brkp);
	for (pi = BrkInts; *pi; dum = *savead++, *(*pi++) = XtrInt(dum)) ;
	for (pp = BrkPtrs; *pp; *(*pp++) = *savead++) ;
	DumpVars("After Restore");
	LockChannels(FALSE);	/* make Input, Output closable again */
}


/*---------------------------------------------------------------------------
    saving and restoring the Prolog state.  On systems  where  character
    pointers  and  other  pointers  have  different  formats,  the first
    argument of fread/fwrite has to be coerced to a  character  pointer,
    hence Fread/Fwrite.
---------------------------------------------------------------------------*/

PTR	PArea[NAreas];		/* original boundaries */
ProLong	RArea[NAreas];		/* relocation constant */
ProLong	LArea[NAreas+1];	/* length of area */

#define Patom	PArea[AtomId]
#define Paux	PArea[AuxId]
#define Ptr		PArea[TrailId]
#define Pheap	PArea[HeapId]
#define Pglb	PArea[GlobalId]
#define Plcl	PArea[LocalId]

#define Ratom	RArea[AtomId]
#define Raux	RArea[AuxId]
#define Rtr		RArea[TrailId]
#define Rheap	RArea[HeapId]
#define Rglb	RArea[GlobalId]
#define Rlcl	RArea[LocalId]

#define Latom	LArea[AtomId]
#define Laux	LArea[AuxId]
#define Ltr		LArea[TrailId]
#define Lheap	LArea[HeapId]
#define Lglb	LArea[GlobalId]
#define Llcl	LArea[LocalId]
#define Lsavep	LArea[NAreas]

void save()	/*  save current prolog state */
{
	FILE *fa;
	int ferr;
#define Fwrite(var,sz,len,fl) Ignore fwrite(CharP(var),sz,len,fl)

	/* open file */
	if (!(fa = fopen(AtomToFile(AtomP(Regs_X->v1ofcf)),"wb"))) {
	    ErrorMess = "! cannot write save-file";
	    Event(IO_ERROR);
	}
	/*  save state */
	savevars();		/* create a break environment */
	Lsavep = savead-CellP(lcl0);
	savev(CellP(BasicAtom), rqrdatoms);
	savev(CellP(BasicFunctor), rqrdfuncs);
	savev(&atprompt, 1);
	savev(&atomfp, 1);
	/*  compute length of different stacks */
	Latom = atomfp-atom0;
	Laux  = vrz-auxstk0;
	Ltr   = Regs_tr-tr0;
	Lheap = HeapTop()-heap0;
	Lglb  = Regs_v1-glb0;
	Llcl  = (savead-CellP(lcl0))+2;
	/*  tag the save state with a magic mark and a version number  */
	Fwrite(savemagic, strlen(savemagic)+1, 1, fa);
	Fwrite(&saveversion, sizeof saveversion, 1, fa);
	Fwrite(LArea, sizeof LArea, 1, fa);
	Fwrite(Origin, sizeof(PTR), NAreas, fa);
	Fwrite(&list10, sizeof list10, 1, fa);
	Fwrite(Heap, HeapHeader, 1, fa);
	Fwrite(&brkp, sizeof brkp, 1, fa);
	Fwrite(&fileerrors, sizeof fileerrors, 1, fa);
	Fwrite(&leash, sizeof leash, 1, fa);
	{
	    register ProLong *pl = LArea;
	    register PTR *ps = Origin;
	    register int i = NAreas;
	    while(--i >= 0) Fwrite(*ps++, sizeof(PTR), *pl++, fa);
	}
    ferr = ferror(fa);
	ferr |= fclose(fa) < 0;
	restvars();
	if (ferr != 0) {
	    ErrorMess = SysError();
	    Event(IO_ERROR);
	}
	chmod(AtomToFile(AtomP(Regs_X->v1ofcf)), 0755);
	/* on 4.1 or 4.2, saved states are executable */
}


/*  The pause while Prolog loads a saved state is quite noticeable, at
    around 2 cpu seconds on a VAX 750.  We would like it to go faster.
    Because the partitions in this run need not be the same size as in
    the run that saved the file, C Prolog has to chase around adding a
    suitable offset to every pointer.  On a byte-addressed machine, or
    a 16-bit word-addressed machine, this involves scaling each offset
    by 4 or 2.  Instead of letting C handle this, we scale the offsets
    ourselves once and for all, and do integer addition instead of PTR
    (or, with the stricter typing, CLAUSEP &c) addition.  These macros
    for type breaking are needed anyway, as p+1 would be any number of
    words now that the types are stricter than PTR everywhere.   There
    is an unfortunate machine dependency here:
	C Prolog ASSUMES that p+n = (typeof p)((long)p+k*n)
    for some type-dependent constant k, where p is a pointer.  This is
    NOT true on many machines, such as the Orion, but usually it fails
    for pointers to 8-bit and 16-bit quantities only.  So long as this
    assumption is true for pointers to 32-bit integers and pointers to
    other pointers, all should be well.
*/
#define	REMAP1(c,n)	*((ProLong*)&(c)) += (n)
#define REMAP2(c,d,n)	*((ProLong*)&(c)) = (ProLong)(d)+(n)
#define REMAPp(c,n)	((c) = (PTR)((ProLong)(c)+(n)))
#define REMAPc(c)	((c) = (CLAUSEP)((ProLong)(c)+Rheap))

/** We can't use the ordinary  type  testing  macros  here,  with  the
    obvious exception of the macros for primitives, because they refer
    to  the  new partition boundaries, not to the boundaries in effect
    when the state was saved.  Wasxxx is thus  like  Isxxx  except  it
    means  old  boundaries.   It  turns  out that IsVar works as well.
    WasPtr means that the thing needs relocating; it is not quite  the
    same  thing  as  !IsPrim because on non-BACKWARDS machines we have
    !IsPrim(nil) but nevertheless it should *not* be relocated!

    If C Prolog were a tagged system, or even if we could recognise an
    empty block, a functor block, a clause block, or a term by looking
    at the first word, we could remap everything in one linear sweep.
    However, while we need a recursive function for remapping terms(it
    is given the address of a pointer to the term), and while we use a
    hairy set of loops to walk over the data base, most of the time is
    spent just reading the file in.  Not using stdio would make it run
    faster, but it would be less portable.
*/

#if	BACKWARDS
#   define WasPtr(c)		SC(c,<,0)
#   define WasAtomic(c)		SC(c,>=,Patom)
#   define WasAux(c)		SC(c,<, Pglb)
#   define WasTr(c)		SC(c,>=,Ptr)	/* &WasAux */
#   define WasHeap(c)		SC(c,>=,Pheap)
#   define WasLcl(c)		SC(c,>=,Plcl)	/* &!WasAux &!WasHeap */
#else  !BACKWARDS
#   define WasPtr(c)		SC(c,>=,Paux)
#   define WasAtomic(c)		SC(c,<, Pheap)
#   define WasAux(c)		SC(c,<, Patom)
#   define WasTr(c)		SC(c,>=,Ptr)
#   define WasHeap(c)		SC(c,< ,Pglb)	/* &!WasAux */
#   define WasLcl(c)		SC(c,>=,Plcl)	/* &!WasAux &!WasHeap */
#endif	BACKWARDS


/*  Remap the source term pointed to by tp.
    It can only be a primitive, an atom, or a pointer to a skeleton.
*/

void remap(tp)
    register PTR *tp;
{
	register PTR t = *tp;

	if (WasAtomic(t)) {
	    if (WasPtr(t)) REMAP2(*tp,t,Ratom);
	} else
	if (!IsVar(t)) {
	    register int n;

	    REMAPp(t, Rheap), *tp = t, tp = CellP(t);
	    REMAP1(*tp, Rheap);
	    n = FunctorP(*tp)->arityoffe;
	    while (--n >= 0) remap(++tp);
	}
}


int restore(sfile)
    char *sfile;
{
	FILE *fa;
#define Fread(var,sz,len,fl) Ignore fread(CharP(var),sz,len,fl)

	/*  Try to open the file. */

	if ((fa = fopen(sfile,"rb")) == NullF) {
	    ErrorMess = SysError();
	    return FALSE;
	}
	/*  Check that it is a saved Prolog state. */
	{
	    char magic[80];
	    Fread(magic, strlen(savemagic)+1, 1, fa);
	    if (strcmp(magic, savemagic) != 0) {
		Ignore sprint(ErrorMess = OutBuf,
		    "! File %s is not a saved Prolog state", sfile);
		return FALSE;
	    }
	}
	/*  Check that the version is correct. */
	{
	    int version;
	    Fread(&version, sizeof version, 1, fa);
	    if (version != saveversion) {
		Ignore sprint(ErrorMess = OutBuf,
		    "! File %s is not compatible with this version of Prolog",
		    sfile);
		return FALSE;
	    }
	}
	/*  Read and check the lengths. */
	{
	    register int a;
	    register ProLong newsize;
	    Fread(LArea, sizeof LArea, 1, fa);
	    for (a = NAreas; --a >= 0; ) {
		newsize = LArea[a] * sizeof (PTR);
		newsize = (((newsize/1024+1)*4)/3)* 1024;  /* 33% extra */
		if (newsize > Size[a]) {
		    ProError("%% Expanding %s from %ldK to %ldK\n",
			AreaName[a], Size[a]/1024, newsize/1024);
		    Size[a] = newsize;
		}
	    }
	    CreateStacks();
	}
	Fread(PArea, sizeof PArea, 1, fa);
	Fread(&list10, sizeof list10, 1, fa);
	Fread(Heap, HeapHeader, 1, fa);
	Fread(&brkp, sizeof brkp, 1, fa);
	Fread(&fileerrors, sizeof fileerrors, 1, fa);
	Fread(&leash, sizeof leash, 1, fa);

	/*  Compute the relocation constants. */
	{
	    register ProLong *Rareap = RArea;
	    register PTR *Originp = Origin, *Pareap = PArea;
	    register int a = NAreas;

	    while (--a >= 0) *Rareap++ = (ProLong)*Originp++ - (ProLong)*Pareap++;
	}
	/*  Extract the stacks from the file. */
	{
		ProLong *Lareap = LArea;
	    PTR *Originp = Origin;
	    int a = -1;

	    while (++a < NAreas) {
		register ProLong length = *Lareap++;
		register PTR *stack = CellP(*Originp++);

		Fread(stack, sizeof *stack, length, fa);
		if (a != AtomId && a != HeapId) {
		    /*  relocate the stack contents */
		    while (--length >= 0) {
			register PTR elem = *stack;
			if (WasPtr(elem)) REMAPp(elem,
			    WasAux(elem) ? (WasTr(elem) ? Rtr : Raux)
			  : WasHeap(elem) ? (WasAtomic(elem) ? Ratom : Rheap)
			  : (WasLcl(elem) ? Rlcl : Rglb));
			*stack++ = elem;
		    }
		}
	    }
	}
	Ignore fclose(fa);
	savead = CellP(lcl0)+Lsavep;
	restv(CellP(BasicAtom), rqrdatoms);
	restv(CellP(BasicFunctor), rqrdfuncs);
	restv(&atprompt, 1);
	restv(&atomfp, 1);
	REMAP1(brkp, Rlcl);
	restvars();
	/*  NB:  the various registers  in  the  Prolog  machine  (those
	    referred  to  in  BrkEnv) have already been remapped because
	    savevars() saves the environment in the local  stack,  whose
	    contents have been remapped above (tricky, this one!)
	*/
	/*  Remap the free space chains. */
	RelocHeap(Rheap);
	remap(&list10);
	SetPlPrompt(AtomP(atprompt)->stofae);
	/*  Remap the atom and heap areas. */
	{
	    PTR hashp, atomp, funcp;
	    register PTR t;
	    register CLAUSEP remapcl;
#define remapfn FunctorP(t)

	    for (hashp = hasha = atom0; hashp < hasha+HashSize; hashp++) {
		/*  Remap this hash chain. */
		for (atomp = hashp;
		    /*while*/ *CellP(atomp) != NullP;
		    /*doing*/ atomp = Addr(AtomP(*atomp)->nxtofae) ) {
		    /*  Remap all the functors for this atom. */
		    for (funcp = atomp;
			/*while*/ t = *CellP(funcp), t != NullP;
			/*doing*/ funcp = Addr(remapfn->nxtoffe) ) {
			if (funcp == atomp) {	/* update atom block */
			    REMAPp(t, Ratom);
			} else {		/* update functor block */
			    REMAPp(t, Rheap);
			    remapfn->gtoffe = t;
			}			/* remapfn is an alias of t */
			*CellP(funcp) = t;	/* update pointer to me */
			REMAP1(remapfn->atoffe, Ratom);

			/*  Remap the clause chain of remapfn, if it has one */
			/*  Frameless primitives have a non-NullC defsoffe  */
			/*  which is nevertheless not a clause, care needed. */
			if (WasPtr(remapcl = remapfn->defsoffe)) {
			    remapfn->defsoffe = REMAPc(remapcl);
			    /*  remap each clause in the chain  */
			    for (;; remapcl = remapcl->altofcl) {
				remap(&(remapcl->hdofcl));
				remap(&(remapcl->bdyofcl));
				if (remapcl->prevofcl != NullC)
				    REMAPc(remapcl->prevofcl);
				if (remapcl->altofcl == NullC) break;	/*END*/
				REMAPc(remapcl->altofcl);
				if (remapcl->altofcl == remapcl) break;	/*LOOP*/
			    }
			}

			/*  Remap the record chain of remapfn, if it has one */
			if ((remapcl = remapfn->dboffe) != NullC) {
			    remapfn->dboffe = REMAPc(remapcl);
			    /*  remap each record */
			    for (;; remapcl = remapcl->altofcl) {
				REMAPp(remapcl->hdofcl,
				    funcp == atomp ? Ratom : Rheap);
				remap(&(remapcl->bdyofcl));
				if (remapcl->prevofcl != NullC)
				    REMAPc(remapcl->prevofcl);
				if (remapcl->altofcl == NullC) break;	/*END*/
				REMAPc(remapcl->altofcl);
			    }
			}
		    }
		}
	    }
	}
	return TRUE;
}

/*----------------------------------------------------------------------+
|									|
|		Miscellaneous functions.				|
|									|
+----------------------------------------------------------------------*/

void ResetTrail()
{
	register PTR *a = CellP(tr0);
	register PTR *b = CellP(Regs_tr);
	DeclRegisters
	InitRegisters

	Regs_tr = (PTR) a;
	while (b != a) {
	    register PTR e = *--b;
	    if (IsRef(e)) {			/* variable to reset */
	    	VarVal(e) = NullP;
	    } else {				/* clause to erase */
	    	register CLAUSEP cl = XtraDB(e);
			if (cl->infofcl & ERASED) hide(e);
			else cl->infofcl &= ~IN_USE;
	    }
	}
}


 PTR bread()
    /*  read initialization terms */
{
	PTR r;

	Regs_V = FrameP(lcl0), Regs_v1 = glb0, vrz = auxstk0, reading = FALSE;
	SetPlPrompt("    >> ");
	do PromptIfUser("boot>> ");
	while (!(r = ProPRead(CellP(0)) ));
	return r;
}


void Halt(why)
    int why;		/* 0->eval pred, 1->debug, 2->interrupt */
{
#if	COUNTING
	int epno;
	double r = 4.0/(double)smpcnt;
#endif	COUNTING

	LockChannels(2);
	CloseFiles();
#if	COUNTING
	Ignore fclose(trace_file);
	ProError("\nAssorted counts.\n");
	ProError("Max Local + Global stack + Trail = %ld + %ld + %ld\n",
	    maxloc*sizeof(PTR), maxglo*sizeof(PTR), maxtr*sizeof(PTR));
	ProError("Avg Local + Global stack + Trail = %ld + %ld + %ld\n",
	    (ProLong)(totloc*r), (ProLong)(totglo*r), (ProLong)(tottr*r) );
	ProError("Call %ld Exit %ld Back %ld Fail %ld Next %ld\n",
	    portct[CALL_PORT], portct[EXIT_PORT],
	    portct[BACK_PORT], portct[FAIL_PORT], portct[NEXT_PORT]);
	ProError("Evaluable predicate counts:\n");
	for (epno = 0; epno < NPREDS; epno++) ProError("%3d %5ld%c",
		epno, instrct[epno], (epno&3)==3 ? '\n' : '\t');
#endif	COUNTING
	ProError("\n%% Prolog execution halted\n");
	exit(why);
}

/*----------------------------------------------------------------------+
|									|
|	The Prolog interpreter.						|
|	It is divided into general initialisation, bootstrap loading,	|
|	the four ports, the debugger interface, and the evaluable	|
|	predicates.  The latter are in their own file.			|
|									|
+----------------------------------------------------------------------*/
int
main(ArgC, ArgV)
    int ArgC;
    char *ArgV[];
    {
	FUNCTORP f;		/* the functor of the current goal (CALL only) */
	int PredNo;		/* index of evaluable predicate */
	char *bn;		/* initial file name, then scratch */
	PTR k;			/* scratch variable */
	int n, i;		/* scratch variables */
	DeclRegisters		/* keep glb0, heap0 in registers */

#if	COUNTING
	trace_file = fopen("trace.txt", "wb");
#endif	COUNTING
	/* our first Prolog event will be a cold start */
	PrologEvent = COLD_START;

	/* Prolog events cause execution to resume fom here */
	Ignore setjmp(ZeroState);
	InitRegisters;	/* in case longjmp clobbered them */
	CatchSignals();	/* prepare to handle signals etc. */

	switch (PrologEvent) {
	    /*  Prolog event handling.  Unix signals are mapped to Prolog events.
		Event(..) can also be used to force a Prolog event to occur.
		For conciseness, many of these cases fall through.
	    */
	    case END_OF_FILE:		/* input ended ('Seen' has been called) */
		if (reading) {
		    k = EndOfFile;	/* return 'end_of_file' */
		    goto resumeread;	/* jump into EvalPred block, UGH */
		}
		ErrorMess = "! Input ended";

	    case IO_ERROR:		/* files error */
		if (fileerrors) goto FAIL;

	    case GEN_ERROR:		/* general error with message requiring abort */
		ProError("\n%s\n", ErrorMess);

	    case ABORT:			/* abort */
aborting:
		ProError("\n\n%% execution aborted\n\n");
		ResetTrail(); LockChannels(2); CloseFiles();
		goto restart;

	    case ARITH_ERROR:		/* error in arithmetic expression */
		ProError("\n! Error in arithmetic expression: %s\n", ErrorMess);
		debug = TRUE, sklev = NEVER_SKIP;
		goto FAIL;

	    case COLD_START:	/* cold start */
		break;
	}
	InitIO();		/* initialise the I/O system */

	/*  process command line parameters  */

	bn = crack(ArgC, ArgV);
	if (!InBoot) {
//	    if(!State[QUIET]) {
	    	ProError("%% Restoring from file %s\n", bn);
//	    }
	    if(restore(bn)) {
	    	InitIO();
	    	InitRegisters;		/*  set up glb0, heap0 registers */
	    	running = TRUE;
	    	ProError("%% Prolog started\n");
	    	/* the system is now up and running */
	    	if (brklev != 0) ProError("%% Restarting break (level %d)\n", brklev);
	    	TRY(unifyarg(Addr(Regs_X->v2ofcf), ConsInt(1), (PTR)0));
	    }
	    ProError("%s\n", ErrorMess);
	    exit(1);
	}

	ProError("%% Bootstrapping session. Initializing from file %s\n", bn);
	CSee(bn);		/* we never need the file name again */

    /*  Create the memory partitions (normally done in restore)  */
	CreateStacks();

    /*  initialise atom area  */
	hasha = atom0, atomfp = atom0;
	for (i = HashSize; --i >= 0; *(ATOMP*)(atomfp++) = AtomP(0)) ;

    /*  initialise read/print vars  */
	lc = TRUE, quoteia = FALSE;

    /*  initialise heap  */
	InitHeap();

    /*  initialise I/O system  */
	fileerrors = FALSE;

    /*  read required atoms  */
	for (i = 0; i < rqrdatoms; i++)
	    BasicAtom[i] = (ATOMP)bread();

    /*  read required functors  */
	for (i = 0; i < rqrdfuncs; i++)
	    BasicFunctor[i] = SkelFuncP(MolP(bread())->Sk);

    /*  create term list10  */
	k = list10 = getsp(27);
	for (i = 9; i > 0; i--) {
	    SkelP(k)->Fn = listfunc,
	    SkelP(k)->Arg1 = SkelGlobal(i),
	    SkelP(k)->Arg2 = k+3,
	    k += 3;
	}
	*CellP(list10+26) = SkelGlobal(0);

    /*  Set top level termination  */
    /*  On success:  */
	Yes->defsofae = ClauseP(_yes_);	/* system pred number */

    /*  On failure:  clause $no :- _no_  */
	Regs_v1 = glb0;
	*CellP(Regs_v1++) = (PTR)No,
	*CellP(Regs_v1++) = ConsInt(_no_);
	ConsMol(Addr(arrowtag->gtoffe), glb0, pg);
	if (!record(CLAUSE, pg, (PTR)0, FALSE)) {
	    ProError("\n! Fatal error in startup - consult wizard\n");
	    Stop(TRUE);
	}
	d = No->defsofae;	/* Fail to itself */
	d->altofcl = d;

	atprompt = atomnil;
	running = TRUE;		/* boot is now running */
	ProError("%% Prolog started\n");

    /*  restart here after an abort etc.  */
restart:
	InitRegisters;		/* glbREG, heapREG */
	vrz = auxstk0, Regs_tr = tr0, Regs_v1 = glb0;
	FileAtom[STDIN] = FileAtom[STDOUT] = useratom;
	dotrace = FALSE, brklev = 0, usermode = FALSE;
	if (!InBoot) {
	    pg = live;
	    goto go;
	}

    /*  main loop during bootstrap session  */
BootLoop:
	pg = bread();
	g = IsaRef(pg) && !IsUnbound(pg) ? VarVal(pg) : pg;
	if (g == EndOfFile) {
	    Seen();
	    InBoot = FALSE;
	    goto restart;
	}

	if (IsaAtomic(g) || SkelFuncP(g) != provefunc) {
	    if (!record(CLAUSE, pg, (PTR)0, FALSE)) {
		int telling = Output;

		ProError("%s\n", ErrorMess);
		Output = STDERR;
		ProPWrite(pg, (PTR) 0, 1200);
		Put('\n');
		Output = telling;
	    }
	    goto BootLoop;
	}

	SetPlPrompt("| ");
	pg = arg(SkelP(g)->Arg1, MolP(pg)->Env);

    /*  go - run the goal pg  */
go:
	{
	    register FRAMEP rV = FrameP(lcl0);

	    Regs_X = rV, Regs_V = rV, Regs_VV = rV;
	    rV->gofcf = (PTR)No,
	    rV->altofcf = &(No->defsofae->altofcl),
	    rV->gfofcf = rV->lcpofcf = rV,
	    rV->gsofcf = Regs_vv1 = Regs_x1 = glb0,
	    rV->trofcf = Regs_tr = tr0,
	    rV->infofcf = FRM0,
	    rV->cofcf = c = (PTR)Yes;
	}
	GrowLocal(szofcf);
	dotrace = FALSE, sklev = NEVER_SKIP,
	lev = 1, invokno = 0, usermode = FALSE;
	goto CALL;

/*----------------------------------------------------------------------+
|									|
|			CALL port					|
|									|
+----------------------------------------------------------------------*/

CALL:
	{
	    register PTR PG = pg;	/* pointer to goal */
	    if (IsaRef(PG)) Regs_x1 = MolP(PG)->Env, PG = MolP(PG)->Sk;
	    g = PG, f = SkelFuncP(PG);
	}
	/*  Now (g,x1) is a molecule describing the goal, and f is the	*/
	/*  principal functor of the goal.  pg can be a skeleton or an	*/
	/*  atom or a molecule.  Skeletons arise from "continuations",	*/
	/*  molecules from proper calls.  pg is at least nonprimitive.	*/

#if	COUNTING
	portct[CALL_PORT]++;
	fprint(trace_file, "%ld %ld %ld %ld\n", Regs_v-lcl0, Regs_v1-glb0, Regs_x-lcl0, Regs_x1-glb0);
#endif	COUNTING

	if (!usermode) {
	    info = lev|FRM0;
	} else {
	    bb = f->flgsoffe;
	    if (!(bb & INVISIBLE)) invokno++;
	    info = (invokno<<LEVEL_WIDTH)|lev|FRM0;

	    if (debug && !(bb & (INVISIBLE|UNTRACEABLE))) {
	    /*  Basic tracing package: trace CALL  */
		port = CALL_PORT;
		if (dotrace) {
		    if (dotrace & 4) {	/* forced "break" */
			debug = (dotrace>>1)&1, dotrace &= 1;
			brtn = 3; savevars(); pg = breakat;
			goto CALL;
		    }
		    dotrace = FALSE, sklev = NEVER_SKIP;
		}
		if (lev <= sklev || (bb&spy)) goto message;
ret_call:;	/*  return here from message  */
		f = SkelFuncP(g);	/* no longer saved */
	    }
	}
	{
	    register CLAUSEP D = f->defsoffe;

	    /*  Note: system predicates have numbers 1..255 stored
		directly, and Null is 0.  These satisfy the IsAtomic
		test, but except on BACKWARDS machines they do NOT
		satisfy the IsPrim test.
	    */
	    if (IsaAtomic(D)) {
		if (D == NullC) {
		    if (!usermode	/* running system code */
		    ||  !unknown	/* not checking */
		    ||  bb&SPY_ME	/* user is spying it */
		    ||  !IsaAtomic(f) && f->moreflgs != 0
		    ) goto FAIL;	/* don't complain */
		    Ignore sprint(ErrorMess = OutBuf, "! %s/%d is undefined",
			f->atoffe->stofae, f->arityoffe);
		    goto ERROR;
		}
		PredNo = Signed(D);	/* 1..255 */
		goto EvalPred;
	    }
    	    while (D->infofcl & ERASED)
		if ((D = D->altofcl) == NullC) goto FAIL;
	    d = D;	/*  d is the first remaining clause for f  */
	}
	{
	    register FRAMEP rV = Regs_V;
	    if ((PTR)rV > vmax) NoSpace(LocalId);
	    rV->gofcf   = pg,
	    rV->gfofcf  = Regs_X,
	    rV->lcpofcf = Regs_VV,
	    rV->gsofcf  = Regs_vv1 = Regs_v1,
	    rV->trofcf  = Regs_tr,
	    rV->infofcf = info,
	    rV->cofcf   = c,
		Regs_VV = rV;
	}
	goto BACK;

/*----------------------------------------------------------------------+
|									|
|			try the next clause				|
|			(CALL+REDO ports)				|
|									|
+----------------------------------------------------------------------*/

BACK:
#if	COUNTING
	portct[BACK_PORT]++;
#endif	COUNTING

	{
	    PTR v1t = Regs_v1;		/* local copy of v1 (which changes) */
#if	USEREGS
	    PTR vt  = v;		/* local copies of v, x, and x1 */
	    PTR x1t = x1;		/* are made on the VAX and Orion */
	    PTR xt  = x;		/* to use short fast addresses */
#else	!USEREGS
#define vt  Regs_v
#define x1t Regs_x1
#define xt  Regs_x
#endif	USEREGS

TryClause:
		/*  Initialse the local and global variables of this clause to NullP  */
	    {
	    	register PTR *vp;
	    	register int  vn;

	    	for (vn = d->gvofcl, vp = CellP(v1t);   --vn >= 0; *vp++ = NullP) ;
	    	Regs_v1 = (PTR)vp;
	    	for (vn = d->ltofcl, vp = &(Regs_V->v1ofcf); --vn >= 0; *vp++ = NullP) ;
	    }

	    // -- cprolog.com -- debug
	    //if(ProCheckClauseTry(d, "$directive")) {
	    //	ProShowClause(d, v1t, vt, 'A');
	    //	ProShowSkel(SkelP(g), x1t, xt, 'A');
	    //}
	    // -- cprolog.com -- debug

	    /*  Try to unify the head of the clause with the goal  */
	    if (!IsaAtomic(g)) {
	    	PTR tb = g;
	    	PTR ta = d->hdofcl;
	    	int arity = SkelFuncP(ta)->arityoffe;
	    	register PTR a, b, pa, pb;

	    	/* main unification loop */

	    	while (--arity >= 0) {
	    		b = NextArg(tb);
	    		a = NextArg(ta);
	    		if (IsaVar(a)) {
	    			pa = FrameVar(a, v1t, vt);
	    			while (IsaRef(a = VarVal(pa))) pa = a;
	    			if (IsaVar(b)) {
	    				pb = FrameVar(b, x1t, xt);
	    				while (IsaRef(b = VarVal(pb))) pb = b;
	    				if (pa == pb) {
	    				} else
	    					if (Undef(b)) {
	    						if (!Undef(a)) {
	    							VarVal(pb) = IsaAtomic(a) ? a : pa;
	    							TrailReg(pb);
	    						} else
	    							if (pa > pb) {
	    								VarVal(pa) = pb;
	    								TrailReg(pa);
	    							} else {
	    								VarVal(pb) = pa;
	    								TrailReg(pb);
	    							}
	    					} else
	    						if (Undef(a)) {
	    							VarVal(pa) = IsaAtomic(b) ? b : pb;
	    							TrailReg(pa);
	    						} else
	    							if (IsaAtomic(a)) {
	    								if (a != b) goto TryNextClause;
	    							} else
	    								if (IsaAtomic(b) || !gunify(a, MolP(pa)->Env, b, MolP(pb)->Env))
	    									goto TryNextClause;
	    			} else
	    				if (Undef(a)) {
	    					if (IsaAtomic(b)) VarVal(pa) = b;
	    					else ConsaMol(b, x1t, pb, pa);
	    					TrailReg(pa);
	    				} else
	    					if (IsaAtomic(b)) {
	    						if (a != b) goto TryNextClause;
	    					} else
	    						if (IsaAtomic(a) || !gunify(a, MolP(pa)->Env, b, x1t))
	    							goto TryNextClause;
	    		} else
	    			if (IsaVar(b)) {
	    				pb = FrameVar(b, x1t, xt);
	    				while (IsaRef(b = VarVal(pb))) pb = b;
	    				if (Undef(b)) {
	    					if (IsaAtomic(a)) VarVal(pb) = a;
	    					else ConsaMol(a, v1t, pa, pb);
	    					TrailReg(pb);
	    				} else
	    					if (IsaAtomic(b)) {
	    						if (a != b) goto TryNextClause;
	    					} else
	    						if (IsaAtomic(a) || !gunify(a, v1t, b, MolP(pb)->Env))
	    							goto TryNextClause;
	    			} else {
	    				if (IsaAtomic(a)) {
	    					if (a != b) goto TryNextClause;
	    				} else
	    					if (IsaAtomic(b) || !gunify(a, v1t, b, x1t))
	    						goto TryNextClause;
	    			}
	    	}
	    }   /* end in-line unification */
	    if (Regs_v1 > v1max) NoSpace(GlobalId);

	    /*  We found a clause whose head matches, so enter it  */
	    {
	    	register CLAUSEP D = d;
	    	register PTR C = D->bdyofcl;
	    	register FRAMEP rV = Regs_V;

		    // -- cprolog.com -- debug
	    	//ProShowClause(D, v1t, vt, 'A');
		    // -- cprolog.com -- debug

	    	rV->altofcf = fl = &(D->altofcl);
	    	if (!debug && D->altofcl == NullC)
	    		Regs_VV = rV->lcpofcf, Regs_vv1 = Regs_VV->gsofcf;
	    	Regs_V = FrameP((PTR)rV + (szofcf + D->lvofcl));

	    	if (!(D->infofcl & IN_USE)) {
	    		D->infofcl |= IN_USE;
	    		TrailPtr(ConsaDB(D, CLAUSE));
	    	}
	    	if (C == NullP) {
	    		pg = rV->gofcf;
	    		goto neckfoot;
	    	}
	    	Regs_X = rV, Regs_x1 = v1t;
	    	if (usermode && !(bb & INVISIBLE)) {
	    		if (bb & PROTECTED) usermode = FALSE; else lev++;
	    	}
	    	if (IsPrim(C)) {
	    		PredNo = XtrByte(C);
	    		c = NullP;
	    		goto EvalPred;
	    	}
	    	if (SkelFuncP(C) == commatag) {
	    		pg = SkelP(C)->Arg1, c = SkelP(C)->Arg2;
	    	} else {
	    		pg = C, c = NullP;
	    	}
	    }
	    goto CALL;

	/*  When the head unification fails, we jump to TryNextClause.   */
	/*  If that finds another candidate, we jump to TryClause.       */
	/*  If not, we proceed to FAIL.  This is "shallow backtracking". */

TryNextClause:
#if	COUNTING
	portct[NEXT_PORT]++;
#endif	COUNTING

	    {	/* Reset the trail */
			register PTR *a = CellP(Regs_V->trofcf), *b = CellP(Regs_tr);
			while (b != a) VarVal(*--b) = NullP;
			Regs_tr = (PTR)a;
	    }
	    {	/* Look for another clause */
	    	register CLAUSEP D = d;
	    	while ((D = D->altofcl) != NullC)
	    		if (!(D->infofcl & ERASED)) {
	    			d = D, Regs_v1 = v1t;
	    			goto TryClause;
	    		}
	    }
	    Regs_VV = Regs_V->lcpofcf;
	    goto FAIL;
	}

/*----------------------------------------------------------------------+
|									|
|			EXIT port					|
|									|
+----------------------------------------------------------------------*/

/*  This seems to be the best place for gathering statistics about the */
/*  stack sizes.  There are two versions of the exit code proper, both */
/*  of them do the same thing, but the non-debugging version should be */
/*  rather faster, especially when exit follows exit as often happens. */

EXIT:
#if	COUNTING
	{   register ProLong t;
	    t = Regs_v  - lcl0; if (t > maxloc) maxloc = t; totloc += t;
	    t = Regs_v1 - glb0; if (t > maxglo) maxglo = t; totglo += t;
	    t = Regs_tr - tr0;  if (t > maxtr ) maxtr  = t; tottr  += t;
	    smpcnt++;
	}
	portct[EXIT_PORT]++;
#endif	COUNTING

	if (!debug) {
	    register FRAMEP rX = Regs_X;
	    register PTR C = c;

	    while (C == NullP) {
	    	if (rX > Regs_VV) Regs_V = rX;
	    	C = rX->cofcf, info = rX->infofcf, rX = rX->gfofcf;
	    }
	    lev = info&LEVEL, usermode = IsVisible(info);
	    Regs_X = rX, Regs_x1 = rX->gsofcf;
	    if (SkelFuncP(C) == commatag) {
	    	pg = SkelP(C)->Arg1, c = SkelP(C)->Arg2;
	    } else {
	    	pg = C, c = NullP;
	    }
	    goto CALL;
	}
	{
	    register FRAMEP rX = Regs_X;
	    register PTR C = c;

	    if (C != NullP) {	/* there is a continuation */
	    	Regs_x1 = rX->gsofcf;
	    	if (SkelFuncP(C) == commatag) {
	    		pg = SkelP(C)->Arg1, c = SkelP(C)->Arg2;
	    	} else {
	    		pg = C, c = NullP;
	    	}
	    	goto CALL;
	    }
	    if (rX > Regs_VV) Regs_V = rX;
	    c = rX->cofcf, pg = rX->gofcf,
	    Regs_X = rX->gfofcf, info = rX->infofcf;
	    lev = info&LEVEL, usermode = IsVisible(info);
	}
neckfoot:
	if (debug && usermode) {
	/*  Basic debugging package: trace EXIT  */
	    register PTR PG = pg;
	    if (IsaRef(PG)) {
	    	g = MolP(PG)->Sk, Regs_x1 = MolP(PG)->Env;
	    } else {
	    	g = PG, Regs_x1 = Regs_X->gsofcf;
	    }
	    port = EXIT_PORT, bb = SkelFuncP(g)->flgsoffe;
	    if ((lev <= sklev || (bb&spy)) && !(bb&(INVISIBLE|UNTRACEABLE)))
	    	goto message;	/* which returns to EXIT */
	}
	goto EXIT;

/*----------------------------------------------------------------------+
|									|
|			FAIL port					!
|									|
+----------------------------------------------------------------------*/

ERROR:
	ProError("\n%s\n", ErrorMess);
	debug = TRUE, sklev = NEVER_SKIP;
cutfail:
	if (Regs_VV >= Regs_X) Regs_VV = Regs_X->lcpofcf;

FAIL:			/*  deep backtracking */
#if	COUNTING
	portct[FAIL_PORT]++;
#endif	COUNTING

	{
	    register FRAMEP rV;		/* local copy of VV */
	    register CLAUSEP D;		/* local copy of d  */

	    if (debug) {
	    	if (usermode) {
	    		/*  Basic debugging package: trace FAIL  */
	    		port = FAIL_PORT, bb = SkelFuncP(g)->flgsoffe;
	    		if ((lev <= sklev || (bb&spy)) && !(bb&(INVISIBLE|UNTRACEABLE)))
	    			goto message;
ret_fail:;	/*  message returns here  */
	    	}
	    	rV = Regs_VV;
	    	for (D = *(rV->altofcf); D != NullC && (D->infofcl & ERASED);
	    	D = D->altofcl) ;
	    	i = !(rV == Regs_X && c == NullP);	/* !fail_parent */
	    } else {	/* ! debug */
	    	for (rV = Regs_VV; ; rV = rV->lcpofcf) {
	    		for (D = *(rV->altofcf); D != NullC && (D->infofcl & ERASED);
	    				D = D->altofcl) ;
	    		if (D != NullC) break;
	    	}
	    }
	    Regs_X = rV->gfofcf, pg = rV->gofcf,
	    c = rV->cofcf, info = rV->infofcf;
	    lev = info&LEVEL, usermode = IsVisible(info),
	    Regs_vv1 = Regs_v1 = rV->gsofcf, Regs_VV = Regs_V = rV, d = D;
	}
    /*	ResetTrail(V->trofcf);  */
	{
	    register PTR *a = CellP(Regs_V->trofcf);
	    register PTR *b = CellP(Regs_tr);

	    Regs_tr = (PTR)a;
	    while (b != a) {
	    	register PTR e = *--b;
	    	if (IsaRef(e)) {
	    		VarVal(e) = NullP;
	    	} else {
	    		register CLAUSEP cl = XtraDB(e);
	    		if (cl->infofcl & ERASED) hide(e);
	    		else cl->infofcl &= ~IN_USE;
	    	}
	    }
	}

    /*  we have arrived back at a likely candidate for retrial  */
    /*  the next thing would be to jump straight to BACK, except */
    /*  that we want to trace this event.  (Almost REDO).	*/

	{
	    register PTR PG = pg;	/* pointer to goal */
	    if (IsaRef(PG)) Regs_x1 = MolP(PG)->Env, PG = MolP(PG)->Sk;
	    else /*atom*/   Regs_x1 = Regs_X->gsofcf;
	    g = PG, bb = SkelFuncP(PG)->flgsoffe;
	}
	if (!debug) goto BACK;		/* already know d != NullC */
	if (i && usermode) {
	/*  Basic debugging package: trace BACK */
	    port = BACK_PORT;
	    if ((lev < sklev || (bb&spy)) && !(bb&(INVISIBLE|UNTRACEABLE)))
		goto message;
ret_back:;  /*  message returns here  */
	}
	if (d != NullC) goto BACK;
	Regs_VV = Regs_VV->lcpofcf;
	goto FAIL;

/*----------------------------------------------------------------------+
|									|
|		A minimal 4-port debugging package.			|
|		This is a quasi-procedure for displaying messages.	|
|		It jumps to ret_call, EXIT, or ret_back, or ret_fail	| 			|
|		<g,x1> is the goal being traced.			|
|									|
+----------------------------------------------------------------------*/

message:
	Ignore sprint(OutBuf, "%c%c (%3ld) %2ld %s: ",
	    bb & SPY_ME ? '*' : ' ',		/* spy-point? */
	    lev == sklev ? '>' : ' ',		/* return to skip? */
	    (info>>LEVEL_WIDTH) & CALL_NUMBER,	/* sequential call number */
	    info & LEVEL,			/* depth of call */
	    portname[port]);			/* which of the 4 ports? */
	{
	    int telling = Output;
	
	    Output = STDOUT;			/* STDERR is not folded drat */
	    PutString(OutBuf);			/* header has to be folded too */
	    ProPWrite(g, Regs_x1, 1200);
	    if (leash & portmask[port]) {	/* ask at this sort of port? */
	    	PutString(" ? ");
	    	Output = telling;
	    } else {				/* just tracing */
	    	Put('\n');
	    	Output = telling;
	    	goto action;
	    }
	}
	switch (ToEOL()) {
	    case '\n':
	    case  'c':					/* creep */
		spy = FALSE, sklev = NEVER_SKIP;
		goto action;
	    case 'l':					/* leap */
		spy = SPY_ME, sklev = 0;
		goto action;
	    case 's':					/* skip */
		if (port == EXIT_PORT || port == FAIL_PORT) {
		    ProError("! can't skip at this port\n");
		    goto message;
		}
		spy = FALSE, sklev = info&LEVEL;
		goto action;
	    case 'q':					/* quasi-skip */
		if (port == EXIT_PORT || port == FAIL_PORT) {
		    ProError("! can't quasi-skip at this port\n");
		    goto message;
		}
		spy = SPY_ME, sklev = info&LEVEL;
		goto action;
#if	_DEFINE_0
	    case 'r':					/* retry */
		spy = FALSE, sklev = NEVER_SKIP;
		goto CALL;			
#endif	0
	    case 'f':					/* fail */
	    	if (Regs_VV >= Regs_X) Regs_VV = Regs_X->lcpofcf;
	    	spy = FALSE, sklev = NEVER_SKIP;
	    	goto FAIL;
	    case 'e':					/* exit */
	    	Halt(0);
	    case 'a':					/* abort */
	    	goto aborting;
	    case 'b':					/* break */
	    	brtn = 2; savevars(); pg = breakat;
	    	goto CALL;
	    case 'g':					/* backtrace */
	    	backtrace();
	    	goto message;
	    case 'n':	/* turn debug mode off */
	    	debug = FALSE;
	    	goto action;
	    default:
	    	ProError("! Unknown option.  Known ones are\n");
	    case 'h':					/* help */
	    case '?':
	        ProError("\
<cr>	creep           a	abort\n\
c	creep           f	fail\n\
l	leap            b	break\n");
		ProError("\
s	skip            h	help\n\
q	quasi-skip      n	nodebug\n\
e	exit prolog	g	write ancestor goals\n");
			goto message;
	}
action:
	switch (port) {
	    case CALL_PORT: goto ret_call;
	    case EXIT_PORT: goto EXIT;
	    case FAIL_PORT: goto ret_fail;
	    case BACK_PORT: goto ret_back;
	}

EvalPred:

//#include "evalp.c"
/************************************************************************
*									*
*		   C Prolog	evalp.c					*
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

/*----------------------------------------------------------------------+
|									|
|			Evaluable Predicates				|
|									|
|   This file contains the definitions of the evaluable predicates.  A	|
|   table of the numbers assigned to them may be found in evalp.h, and	|
|   the numbers assigned to the predicates  in  pl/init  must  be  the	|
|   same.   There are two sorts of evaluable predicates, ones which do	|
|   not have a stack frame built for them -- defined by $sysp(P,N)  --	|
|   and those which do have a frame -- defined by P:-N -- and most are	|
|   the latter sort.  The predicates fall into 6 major groups:		|
|       group 1: control predicates					|
|       group 2: arithmetic predicates					|
|       group 3: metalogical predicates					|
|       group 4: data base predicates					|
|       group 5: input/output and other file-system access		|
|       group 6: flags of various sorts					|
|   As part of an effort to make C Prolog faster, I have put  as  many	|
|   of  the  local  variables  as  possible into registers below.  The	|
|   Prolog X register is used so often  that  it  seemed  worth  while	|
|   making  it  into  a C register as well.  A number of other changes	|
|   were made as well, so I am not sure what the effect of this change	|
|   is on its own.  heap0 and glb0 are kept in registers all the time.	|
|									|
+----------------------------------------------------------------------*/

    {
	register FRAME *rX = Regs_X;
#define constarg(v,c) unifyarg(Addr(rX->v), c, NullP)

#ifdef	COUNTING
	instrct[PredNo]++;
#endif	COUNTING
	switch (PredNo) {


/*----------------------------------------------------------------------+
|									|
|		Group 1: control predicates				|
|									|
+----------------------------------------------------------------------*/

	case _and_:		/* conjunction: A,B */
	    {
		register PTR conjunct = arg(SkelP(g)->Arg2, Regs_x1);
		register PTR t = Regs_v1;

		MolP(t)->Sk = Addr(HiddenCall->gtoffe),
		MolP(t)->Env= t+2,
		*CellP(t+2) = conjunct,
		conjunct = t, t += 3;
		if (c != NullP) {
		    SkelP(t)->Fn   = commatag,
		    SkelP(t)->Arg1 = conjunct,
		    SkelP(t)->Arg2 = c,
		    c = t, t += 3;
		} else {
		    c = conjunct;
		}
		Regs_v1 = t;
	    }
	    /* there is no break between this case and the next */

	case _hidden_call_:	/* $hidden_call(Goal) */
	    {
		register PTR goal = arg(SkelP(g)->Arg1, Regs_x1);

		if (IsPrim(goal)) goto FAIL;
		if (IsUnbound(goal)) {
		    ErrorMess = "! unbound goal";
		    goto ERROR;
		}
		pg = goal;
		goto CALL;
	    }

	case _user_call_:	/* revive tracing (for setof, etc.) */
	    usermode = TRUE, lev++;
	    /* there is no break between this case and the next */

	case _call_:		/* call(Goal) */
	    {
		register PTR goal = rX->v1ofcf;

		if (IsPrim(goal)) goto FAIL;
		if (IsUnbound(goal)) {
		    ErrorMess = "! call/1: unbound goal";
		    goto ERROR;
		}
		pg = goal;
		goto CALL;
	    }

	case _user_exec_:	/* $user_exec(CommandOrQuestion) */
	    {
		register PTR goals = rX->v1ofcf;

		if (IsPrim(goals)) goto FAIL;
		if (IsUnbound(goals)) {
		    ErrorMess = "! unbound goal";
		    goto ERROR;
		}
		c = goals;
		GrowLocal(1);	/* argument was temporary */
		sklev = 0, lev = 1, invokno = 0, usermode = TRUE;
		info = (1<<LEVEL_WIDTH)|1|FRM0;
		/*  This is a quick patch, not a principled correction  */
		if (debug) {
		    spy = SPY_ME;
		    if (dotrace) sklev = NEVER_SKIP, dotrace = FALSE;
		}
		goto EXIT;
	    }

	case _call1_:		/*  $call(Goal) */
	    {
		register PTR goal = arg(SkelP(g)->Arg1, Regs_x1);

		c = rX->cofcf, Regs_X = rX->gfofcf;
		if (IsPrim(goal)) goto FAIL;
		if (IsUnbound(goal)) {
		    ErrorMess = "! $call/1: unbound goal";
		    goto ERROR;
		}
		pg = goal;
		goto CALL;
	    }

	case _cut_:		/* cut: ! */
	    if (rX > Regs_VV) goto EXIT;	/* determinate already */
	    Regs_VV = rX->lcpofcf;
	    Regs_vv1 = Regs_VV->gsofcf,
   		Regs_V = FrameP((PTR)rX + szofcf + ClauseP((PTR)(rX->altofcf)-(Addr(NullC->altofcl)-NullP))->ltofcl);
	    /*  All this dodging around is because X->altofcf points not
		to the current clause, but to the successor  pointer  of
		the  current  clause.   So  we  have to subtract off the
		offset to get back to the current clause before  we  can
		find out how many local variables we had.
	    */
	    if (rX->trofcf != Regs_tr) {
		register PTR *old_tr = CellP(rX->trofcf);
		register PTR *new_tr = old_tr;
		register PTR tr_entry;
		while (old_tr != CellP(Regs_tr)) {
		    tr_entry = *old_tr++;
#if	BACKWARDS
		    if (!IsaRef(tr_entry) || tr_entry < vv1
		    || tr_entry >= lcl0 && tr_entry < vv)
			*new_tr++ = tr_entry;
#else  ~BACKWARDS
		    /* This is an optimised version of the test above */
		    if (SC(tr_entry,<,Regs_vv1) || tr_entry >= lcl0 && tr_entry < Regs_vv)
			*new_tr++ = tr_entry;
#endif	BACKWARDS
		}
		Regs_tr = (PTR)new_tr;
	    }
	    if (c == NullP) {
		c = rX->cofcf, pg = rX->gofcf,
		Regs_X = rX->gfofcf, info = rX->infofcf;
		lev = info&LEVEL, usermode = IsVisible(info);
		goto neckfoot;
	    }
	    goto EXIT;

	case _repeat_:		/* repeat */
		Regs_VV = rX, Regs_vv1 = Regs_x1, *fl = d;
	    goto EXIT;

	case _abort_:		/* abort */
	    goto aborting;

	case _fail_:		/* fail */
	    goto FAIL;
	    /*  Once upon a time this had no definition, and thus failed.
	      However, when the unknown/2 flag was introduced, it used
	      to turn up all the time as an undefined predicate.  This
	      also gives us a chance to collect statistics on its use.
	    */

	case _halt_:		/* halt */
	    Halt(0);		/* does not return */

	case _yes_:		/* top level success in bootstrap */
	    ResetTrail();
	    goto BootLoop;

	case _no_:		/* top level failure in bootstrap */
	    PutString("no\n");
	    goto BootLoop;

	case _break_:		/* $break(Goal) */
	    {
#if	_DEFINE_0
		This is called in just three places in pl/init.  In each
		place it is an atom or compound term, so there is no need
		to check.  If you change C-Prolog so that that no longer
		holds, reinstate the following tests.

		register PTR goal = arg(SkelP(g)->Arg1, x1);

		if (IsPrim(goal)) goto FAIL;
		if (IsUnbound(goal)) {
		    ErrorMess = "! $break/1: unbound goal";
		    goto ERROR;
		}
		k = goal;
#endif	0
		k = arg(SkelP(g)->Arg1, Regs_x1);
		brtn = 1; savevars(); pg = k;
		dotrace = debug = spy = FALSE;
		goto CALL;
	    }

	case _break_start_:	/* $break_start */
	    ProError("[ Break (level %d) ]\n", brklev);
	    goto EXIT;

	case _break_end_:	/* $break_end */
	    ProError("[ End break (level %d) ]\n", brklev);
	    goto EXIT;

	case _exit_break_:	/* $exit_break */
	    restvars();
	    switch (brtn) {
		case 1: goto EXIT;
		case 2: goto message;
		case 3: goto CALL;
	    }
	    abort();

	case _repply_:		/* $repply */
	    Put(' ');		/*  the (Prolog) caller has to set Output  */
	    for (;;) switch (ToEOL()) {
		case '?':
		case 'h':
			ProError("t switches tracing on, n switches it off\n");
			ProError("Type ; or y for next answer: ");
			continue;
		case 't':
			debug = dotrace = TRUE;
			goto FAIL;
		case 'n':
			debug = dotrace = FALSE;
			goto FAIL;
		case ';':
		case 'y':
			goto FAIL;
		default:
			goto EXIT;
	    }


/*----------------------------------------------------------------------+
|									|
|		Group 2: arithmetic predicates				|
|									|
+----------------------------------------------------------------------*/

	case _ncompare_+EQ:	/*  Lhs =:= Rhs  */
	case _ncompare_+NE:	/*  Lhs =/= Rhs  */
	case _ncompare_+LT:	/*  Lhs  <  Rhs  */
	case _ncompare_+GT:	/*  Lhs  >  Rhs  */
	case _ncompare_+LE:	/*  Lhs =<  Rhs  */
	case _ncompare_+GE:	/*  Lhs  >= Rhs  */
	    TRY(numcompare(PredNo-_ncompare_, Addr(rX->v1ofcf), Addr(rX->v2ofcf)));

	case _lseq_:		/*  lseq(Lhs, Rhs) */
	    {
		register PTR lhs = rX->v1ofcf;
		register PTR rhs = rX->v2ofcf;

		if (IsaRef(lhs) && IsUnbound(lhs) ?
		    IsaRef(rhs) && IsUnbound(rhs) || IsInt(rhs)
		  : IsaRef(rhs) && IsUnbound(rhs) && IsInt(lhs)) {
		    ErrorMess = "! lseq/2: instantiation fault";
		    goto ERROR;
		}
		TRY(IsInt(lhs) && IsInt(rhs) && XtrInt(lhs) <= XtrInt(rhs));
	    }

	case _succ_:		/*  succ(Pred,Succ)  */
	    {
		register PTR argone = rX->v1ofcf;
		register PTR argtwo = rX->v2ofcf;

		if (IsPosInt(argone)) {
		    register ProLong succ = XtrPosInt(argone)+1;
		    TRY(constarg(v2ofcf, ConsInt(succ)));
		} else
		if (IsPosInt(argtwo)) {
		    register ProLong pred = XtrPosInt(argtwo)-1;
		    if (pred >= 0) { k = ConsInt(pred); goto unifyatom; }
		}
		if (IsaRef(argone) && IsUnbound(argone)
		&&  IsaRef(argtwo) && IsUnbound(argtwo)) {
		    ErrorMess = "! succ/2: instantiation fault";
		    goto ERROR;
		}
		goto FAIL;
	    }

	case _is_:		/*  Var is Expr */
	    k = numeval(Addr(rX->v2ofcf));
unifyatom:			/* unify atom k with arg in v1ofcf */
	    {
		register PTR argone = rX->v1ofcf;

		if (IsaRef(argone) && IsUnbound(argone)) {
		    VarVal(argone) = k;
		    TrailReg(argone);
		    goto EXIT;
		}
		TRY(argone == k);
	    }

	case _is_+1:		/*  is(Var,Op,Rhs)  */
	case _is_+2:		/*  is(Var,Op,Lhs,Rhs)  */
	    {
		register PTR op = rX->v2ofcf;
		if (IsPrim(op) || !IsAtomic(op)) {
		    ErrorMess = "! is/3-4: arg 2 not an atom";
		    goto ERROR;
		}
		MolP(Regs_v1)->Sk  = Addr(fentry(AtomP(op), PredNo-_is_)->gtoffe);
		MolP(Regs_v1)->Env = Addr(rX->v3ofcf);
		k = numeval(Addr(Regs_v1));
		goto unifyatom;
	    }

	case _plus_:		/* plus(Addend,Augend,Sum) */
#define getintarg(a,f) a = rX->f;   if (IsaAtomic(a)) {if (!IsInt(a)) goto FAIL;} else {if (!IsUnbound(a)) goto FAIL; n++;}
#define tryintarg(f,b,op,c)	    constarg(f, ConsInt(XtrInt(b) op XtrInt(c)))
	    {
		register PTR p, q, r;
		n = -1;
		getintarg(p, v1ofcf)
		getintarg(q, v2ofcf)
		getintarg(r, v3ofcf)
		if (n > 0) {	/* two or more variables */
		    ErrorMess = "! plus/3: instantiation fault";
		    goto ERROR;
		}
		TRY(IsaRef(p) ? tryintarg(v1ofcf, r,-,q)
		  : IsaRef(q) ? tryintarg(v2ofcf, r,-,p)
		  :		tryintarg(v3ofcf, p,+,q) );
	    }


/*----------------------------------------------------------------------+
|									|
|		Group 3: meta-logical predicates			|
|		Subgroups: type tests, comparison, name, term hacking	|
|									|
+----------------------------------------------------------------------*/

	/*	Group 3.1: type tests	*/

	case _var_:		/* var(Var) */
	    {	register PTR argone = rX->v1ofcf;
		TRY( (IsaRef(argone) && IsUnbound(argone)) );
	    }

	case _nonvar_:		/* nonvar(Var) */
	    {	register PTR argone = rX->v1ofcf;
		TRY(!(IsaRef(argone) && IsUnbound(argone)) );
	    }

	case _integer_:		/* integer(Var) */
	    TRY(IsInt(rX->v1ofcf));

	case _number_:		/* number(Var) */
	    TRY(IsNumber(rX->v1ofcf));

	case _primitive_:	/* primitive(Var) */
	    TRY(IsPrim(rX->v1ofcf));

	case _db_reference_:	/* db_reference(Var) */
	    TRY(IsDBRef(rX->v1ofcf));

	case _atomic_:		/* atomic(Var) */
	    TRY(IsaAtomic(rX->v1ofcf));

	case _atom_:		/* atom(Var) */
	    {	register PTR argone = rX->v1ofcf;
		TRY(!IsPrim(argone) && IsaAtomic(argone));
	    }

	/*	Group 3.2: comparison	*/

	case _kcompare_:	/* $compare(R, T1, T2, N) */
	    /*  This is a hack for the sorting routines.  It lets us
		compare corresponding arguments of two terms without
		taking the terms apart by pattern matching.  We save
		two things: time and global stack space, both vital.
	    */
	    {
		register PTR a, b;
		int WhichArg;

		a = rX->v4ofcf;	/* argument number */
		if (!IsPosInt(a)) goto FAIL;
		if ((WhichArg = XtrPosInt(a)) == 0) {
		    k = (PTR)acompare();
		    goto unifyatom;
		}
		b = rX->v3ofcf;	/* second term */
		if (!IsaRef(b) || IsUnbound(b)) goto FAIL;
		a = rX->v2ofcf;	/* first term */
		if (!IsaRef(a) || IsUnbound(a)) goto FAIL;
		b = (PTR)kcompare(MolP(a)->Sk, MolP(a)->Env,
				  MolP(b)->Sk, MolP(b)->Env, WhichArg);
		TRY(b != NullP && constarg(v1ofcf, b));
	    }

	case _compare_:		/* compare(Op,T1,T2) */
	    k = (PTR)acompare();
	    goto unifyatom;

	case _tcompare_+EQ:	/* T1 == T2 */
	    TRY(icompare() == 0);

	case _tcompare_+NE:	/* T1 \== T2 */
	    TRY(icompare() != 0);

	case _tcompare_+LT:	/* T1 @< T2 */
	    TRY(icompare() < 0);

	case _tcompare_+GT:	/* T1 @> T2 */
	    TRY(icompare() > 0);

	case _tcompare_+LE:	/* T1 @=< T2 */
	    TRY(icompare() <= 0);

	case _tcompare_+GE:	/* T1 @>= T2 */
	    TRY(icompare() >= 0);

	/*	Group 3.3: name	*/

	case _name_:		/*  name(Atom, String) */
	    k = rX->v1ofcf;
	    if (!IsaAtomic(k)) {
		if (!IsUnbound(k)) goto FAIL;
		if (!list_to_string(rX->v2ofcf, OutBuf, 255)) {
		    ErrorMess = "! name/2: argument 2 is not a string";
		    goto ERROR;
		}
		bn = OutBuf;
		if (!NumberString(&bn, &k, FALSE)) k = (PTR)lookup(OutBuf);
		goto unifyatom;
	    }
	    {
		register PTR  list = Regs_v+2;
		register PTR *cons = CellP(list);
		register char *str;

		if (!IsPrim(k)) {	/* atom */
		    str = AtomP(k)->stofae;
		} else
		if (IsNumber(k)) {	/* integer or float */
		    str = num2chars(k);
		} else {		/* data base reference */
		    goto FAIL;
		}
		while (*str) *cons++ = ConsInt(*str++);
		if (cons == CellP(list)) {
		    list = atomnil;
		} else {
		    *cons++ = atomnil;
		    list = makelist(cons-CellP(list), CellP(list)), Regs_v1 -= 2;
		}
		TRY(unifyarg(Addr(rX->v2ofcf), MolP(list)->Sk, MolP(list)->Env));
	    }

	/*	Group 3.4: term hacking	*/

	case _functor_:		/* functor(Term, Functor, Arity) */
	    {	register PTR term = rX->v1ofcf;
		if (IsaAtomic(term)) {
		    TRY(constarg(v2ofcf, term)
		    &&  constarg(v3ofcf, ConsInt(0)) );
		}
		if (!IsUnbound(term)) {
		    register FUNCTORP fn = SkelFuncP(MolP(term)->Sk);
		    TRY(constarg(v2ofcf, (PTR)(fn->atoffe))
		    &&  constarg(v3ofcf, ConsInt(fn->arityoffe)) );
		}
	    }
	    /*  Term is a variable  */
	    ErrorMess = "! functor/3: instantiation fault";
	    {   register PTR fsymbol = rX->v2ofcf;
		if (IsaRef(fsymbol)) {
		    if (IsUnbound(fsymbol)) goto ERROR;
		    goto FAIL;
		}
		if (IsPrim(fsymbol)) {
		    TRY(constarg(v1ofcf, fsymbol)
		    &&  constarg(v3ofcf, ConsInt(0)) );
		}
	    }
	    /*  Term is a variable and Functor is an atom  */
	    {   register PTR aritint = rX->v3ofcf;
		register int arity;
		PTR frame;

		if (IsaRef(aritint) && IsUnbound(aritint)) goto ERROR;
		if (!IsPosInt(aritint)) goto FAIL;
		arity = XtrPosInt(aritint);
		if (arity == 0) {
		    k = rX->v2ofcf;
		    goto unifyatom;
		}
		if (arity > MaxArity) {
		    ErrorMess = "! functor/3: arity > 200 unsupported";
		    goto ERROR;
		}
		InitGlobal(arity, frame);
		TRY(unifyarg(Addr(rX->v1ofcf),
		    Addr(fentry(AtomP(rX->v2ofcf), arity)->gtoffe), frame));
	    }

	case _arg_:		/* arg(ArgNo, Term, Arg) */
	    {
		register int argno;
		register PTR term;
		PTR frame;		/* &frame used, so may not be reg */

		ErrorMess = "! arg/3: instantiation fault";
		term = rX->v1ofcf;	/* arg 1 is the argument number */
		if (IsaRef(term) && IsUnbound(term)) goto ERROR;
		if (!IsPosInt(term)) goto FAIL;
		argno = XtrPosInt(term);
		term = rX->v2ofcf;	/* arg 2 is the term to take apart */
		if (!IsaRef(term)) goto FAIL;
		if (IsUnbound(term)) goto ERROR;
		frame = MolP(term)->Env, term = MolP(term)->Sk;
		if (argno > SkelFuncP(term)->arityoffe) goto FAIL;
		term = argv((PTR)(term[argno]), frame, &frame);
		TRY(unifyarg(Addr(rX->v3ofcf), term, frame));
	    }

	case _univ_:		/*  Term =.. List */
	    {
		register PTR term = rX->v1ofcf;
		register PTR cons = Regs_v+1;
		register int arity;
		PTR env;

		if (IsaRef(term) && IsUnbound(term)) {
		    /* construct a new term from a given list */

		    term = rX->v2ofcf, arity = -1;
		    if (!IsaRef(term) || IsUnbound(term)) goto FAIL;
		    env = MolP(term)->Env, term = MolP(term)->Sk;
		    while (term != atomnil) {
			if (IsaAtomic(term) || IsVar(term) || SkelFuncP(term) != listfunc)
			    goto FAIL;
			if (++arity > MaxArity) {
			    ErrorMess = "! =../2: list too long";
			    goto ERROR;
			}
			NextArg(cons) = arg(SkelP(term)->Arg1, env);
			term = argv(SkelP(term)->Arg2, env, &env);
		    }
		    term = *CellP(Regs_v+2);
		    if (!IsaAtomic(term)) goto FAIL;
		    if (arity == 0) TRY(constarg(v1ofcf, term));
		    if (IsPrim(term)) goto FAIL;
		    term = apply(AtomP(term), arity, CellP(Regs_v+3));
		    Regs_v1 -= 2;
		    TRY(unifyarg(Addr(rX->v1ofcf), MolP(term)->Sk, MolP(term)->Env));
		} else {
		    /* form a list from a given term */
			ProLong nElems = 2;
		    if (IsaRef(term)) {
			env = MolP(term)->Env, term = MolP(term)->Sk;
			NextArg(cons) = (PTR)SkelAtomP(term);
			arity = SkelFuncP(term)->arityoffe;
			nElems += arity;
			while (--arity >= 0)
			    NextArg(cons) = arg(NextArg(term), env);
		    } else {
			NextArg(cons) = term;
		    }
		    NextArg(cons) = atomnil;
		    term = makelist(nElems, CellP(Regs_v+2));
		    Regs_v1 -= 2;
		    TRY(unifyarg(Addr(rX->v2ofcf), MolP(term)->Sk, MolP(term)->Env));
		}
	    }


/*----------------------------------------------------------------------+
|									|
|		Group 4: data base access				|
|									|
+----------------------------------------------------------------------*/

	case _z1_assert_:	/* assertz(C), assert(C) */
	    TEST(record(CLAUSE, rX->v1ofcf, NullP, FALSE), EXIT, ERROR);

	case _a1_assert_:	/* asserta(C) */
	    TEST(record(CLAUSE, rX->v1ofcf, NullP, TRUE),  EXIT, ERROR);

	case _z2_assert_:	/* assertz(C,Ref), assert(C,Ref) */
	case _a2_assert_:	/* asserta(C,Ref) =z2assert+1*/
	    {
		register PTR ref =
		    record(CLAUSE, rX->v1ofcf, NullP, PredNo-_z2_assert_);
		if (ref == NullP) goto ERROR;
		XtraDB(ref)->infofcl |= IN_USE;
		TrailPtr(ref);	/* Trail the clause so it will not vanish */
		TRY(constarg(v2ofcf, ref));
	    }

	case _recordz_:		/* recordz(K,T,Ref) */
	case _recorda_:		/* recorda(K,T,Ref) =recordz+1 */
	    {
		register PTR ref =
		    record(RECORD, rX->v2ofcf, rX->v1ofcf, PredNo-_recordz_);
		if (ref == NullP) goto ERROR;
		XtraDB(ref)->infofcl |= IN_USE;
		TrailPtr(ref);	/* Trail the clause so it will not vanish */
		TRY(constarg(v3ofcf, ref));
	    }

	case _assertr_:		/* $assertr(Clause) */
	    /*  This predicate should ONLY be called from $assertr.	 */
	    /*  It has been tweaked to give just the right error action  */
	    /*  for that particular case, and requires the rest of that  */
	    /*  Prolog definition to finish the error message.		 */
	    if (record(CLAUSE, rX->v1ofcf, (PTR)recons, FALSE)) goto EXIT;
	    ProError("\n%s\n", ErrorMess);
	    goto FAIL;

	case _instance_:	/* instance(Ref,T) */
	    {	register int ans = instance(rX->v1ofcf, Addr(rX->v2ofcf));
		if (ans < 0) {
		    ErrorMess = "! instance/2: 1st argument must be a dbref";
		    goto ERROR;
		}
		TRY(ans);
	    }

	case _erase_:		/* erase(Ref) */
	    TEST(erase(rX->v1ofcf), EXIT, ERROR);

	case _erased_:		/* erased(Ref) */
	    {	register int ans = erased(rX->v1ofcf);
		if (ans < 0) goto ERROR;
		TRY(ans);
	    }

	case _clause_:		/* $clause(P,Ref,_) */
	    {	register PTR key = rX->v3ofcf;
		if (IsaRef(key)) key = MolP(key)->Sk;
		if (SkelFuncP(key)->flgsoffe & RESERVED) goto cutfail;
		rX->v4ofcf = Addr(SkelFuncP(key)->defsoffe);
	    }			/* fall into next clause */
	    FailToSelf;		/* which is its own successor */
	case _clause_+1:
	    k = recorded(CLAUSE);
	    if (k == NullP) goto cutfail;
	    TRY(instance(k, Addr(rX->v1ofcf)));

	case _recorded_:	/* $recorded(T,Ref,K) */
	    {	register PTR key = rX->v3ofcf;
		if (IsaRef(key)) key = MolP(key)->Sk;
		rX->v4ofcf = Addr(SkelFuncP(key)->dboffe);
	    }			/* fall into next clause */
	    FailToSelf;		/* which is its own successor */
	case _recorded_+1:
	    TEST(recorded(RECORD) == NullP, cutfail, EXIT);

	case _catom_:		/* current_atom(Atom) */
	    /* this is called with arg1 unbound */
	    rX->v2ofcf = ConsInt(HashSize), rX->v3ofcf = NullP;
	    FailToSelf;
	case _catom_+1:
	    GrowLocal(3);	/* arguments were classified as temporary */
	    {
		register int slot;	/* which chain to scan */
		register PTR atom;	/* where in chain */

		for ( atom = rX->v3ofcf, slot = XtrInt(rX->v2ofcf)
		    ; atom == NullP
		    ; atom = *CellP(hasha + slot) )
		    if (--slot < 0) goto cutfail;
		rX->v2ofcf = ConsInt(slot),
		rX->v3ofcf = (PTR)(AtomP(atom)->nxtofae);
		Ignore constarg(v1ofcf, atom);	/* MUST succeed */
		goto EXIT;
	    }

	case _cfunctor_:	/*  $current_functor(+A,?N,+Key,+Mask) */
	    rX->v5ofcf = rX->v1ofcf;
	    FailToSelf;
	case _cfunctor_+1:
	    GrowLocal(5);	/* arguments were classified as temporary */
	    {
		auto     int proc = Signed(rX->v3ofcf)&256;	/* pred wanted? */
		register int flgs = XtrByte(rX->v3ofcf);	/* flags wanted */
		register int mask = XtrByte(rX->v4ofcf);	/* mask given */
		register FUNCTORP func = FunctorP(rX->v5ofcf);

		if (func == FunctorP(0)) goto cutfail;
		while ((func->flgsoffe & mask) != flgs
		    || proc && func->defsoffe == NullC
		    || !constarg(v2ofcf, ConsInt(func->arityoffe))
		) {
		    func = func->nxtoffe;
		    if (func == FunctorP(0)) goto cutfail;
		}
		rX->v5ofcf = (PTR)(func->nxtoffe);
		goto EXIT;
	    }

	case _abolish_:		/* abolish(Functor, Arity) */
	    if (!IsaAtomic(rX->v1ofcf) || IsPrim(rX->v1ofcf)) {
		ErrorMess = "! abolish/2: arg 1 not an atom";
		goto ERROR;
	    }
	    if (!IsPosInt(rX->v2ofcf)) {
		ErrorMess = "! abolish/2: arg 2 not a non-negative integer";
		goto ERROR;
	    }
	    abolish(fentry(AtomP(rX->v1ofcf), (int)XtrPosInt(rX->v2ofcf)), TRUE);
	    goto EXIT;


/*----------------------------------------------------------------------+
|									|
|		Group 5: input/output and other file system access	|
|		Subgroups: opening/closing, input, output, other, flags	|
|									|
+----------------------------------------------------------------------*/

	/*	Group 5.1: opening and closing files	*/

	case _see_:		/* see(File) */
	    See(AtomP(rX->v1ofcf));
	    goto EXIT;

	case _seeing_:		/* seeing(File) */
	    k = (PTR)Seeing();
	    goto unifyatom;

	case _2seeing_:		/* seeing(OldFile, NewFile) */
	    k = (PTR)Seeing();
	    if (!constarg(v1ofcf, k)) goto cutfail;
	    GrowLocal(3);	/* add a new argument not reset on failure */
	    rX->v3ofcf = k;	/* remember the old file name */
	    See(AtomP(vvalue(Addr(rX->v2ofcf), &k)));
	    goto EXIT;
	case _3seeing_:		/* what to do on backtracking */
	    See(AtomP(rX->v3ofcf));
	    goto FAIL;

	case _seen_:		/* seen */
	    Seen();
	    goto EXIT;

	case _tell_:		/* tell(File) */
	case _append_:		/* append(File) */
	    Tell(AtomP(rX->v1ofcf), PredNo-_tell_);
	    goto EXIT;

	case _telling_:		/* telling(File) */
	    k = (PTR)Telling();
	    goto unifyatom;

	case _2telling_:	/* telling(OldFile, NewFile) */
	    k = (PTR)Telling();
	    if (!constarg(v1ofcf, k)) goto cutfail;
	    GrowLocal(3);	/* add a new argument not reset on failure */
	    rX->v3ofcf = k;	/* remember the old file name */
	    Tell(AtomP(vvalue(Addr(rX->v2ofcf), &k)), FALSE);
	    goto EXIT;
	case _3telling_:	/* what to do on backtracking */
	    Tell(AtomP(rX->v3ofcf), FALSE);
	    goto FAIL;

	case _told_:		/* told */
	    Told();
	    goto EXIT;

	case _close_:		/* close(File) */
	    PClose(AtomP(rX->v1ofcf));
	    goto EXIT;

	case _ttyflush_:	/* ttyflush(File) */
	    Flush(rX->v1ofcf);	/* 0 means all tty files */
	    goto EXIT;

	/*	Group 5.2: input	*/

	case _read_:		/*  read(Term) */
	case _read2_:		/*  read(Term,Vars) */
	    {
		static PTR vars;

		reading = TRUE, vars = atomnil;
		k = ProPRead(PredNo == _read_ ? CellP(0) : &vars);
resumeread:
	    /*  Come back here when end of file is trapped.  Because end of
		file is handled so badly, we end up jumping into a block! I
		think it is a bit disgusting of C to let us get away with a
		dubious dodge like that.  The upshot is that we have to use
		X instead of rX.  I found this out the hard way!  If we can
		get rid of this jump it would be a good thing.
	    */
		reading = FALSE;
		if (k == NullP) goto FAIL;
		TRY( (PredNo==_read_ || unifyarg(Addr(Regs_X->v2ofcf),vars,NullP))
		   && unifyarg(Addr(Regs_X->v1ofcf), k, NullP));
	    }

	case _get0_:		/* get0(Char) */
	    k = ConsInt(Get());
	    goto unifyatom;

	case _get_:		/* get(Char) */
	    {	register int ch;

		do ch = Get(); while (ch <= ' ' && ch != CtrlZ || ch >= 127);
		k = ConsInt(ch);
	    }
	    goto unifyatom;

	case _skip_:		/* skip(Char) */
	    {	register int ch = intval(Addr(rX->v1ofcf));
		while (Get() != ch) ;	/* end of file is trapped in Get */
	    }
	    goto EXIT;

	case _curlineno_:	/* current_line_number(File, LineNo) */
	    {
		register int L = CurLineNo(AtomP(rX->v1ofcf));
		TRY(L > 0 && constarg(v2ofcf, ConsInt(L)) );
	    }

	/*	Group 5.3: output	*/

	case _display_:		/* display(Term) */
	case _write_:		/* write(Term) */
	case _writeq_:		/* writeq(Term) */
	    quoteia = PredNo-_write_;
	    ProPWrite(rX->v1ofcf, Regs_x1, 1200);
	    goto EXIT;

	case _nl_:		/* nl */
	    Put('\n');
	    goto EXIT;

	case _put_:		/* put(Char) */
	    Put((int)(intval(Addr(rX->v1ofcf))&127));
	    goto EXIT;

	case _tab_:		/* tab(N) */
	    {	register int spaces;

		spaces = intval(Addr(rX->v1ofcf));
		while (--spaces >= 0) Put(' ');
	    }
	    goto EXIT;

	case _xprompt_:		/* $prompt(PromptAtom) */
	    if (Input == STDIN && Output == STDOUT) {
		char *prompt = AtomP(rX->v1ofcf)->stofae;

		if (brklev != 0) {
		    Ignore sprint(OutBuf, "[%d] %s", brklev, prompt);
		    prompt = OutBuf;
		}
		PromptIfUser(prompt);
	    }
	    goto EXIT;

	/*	Group 5.4: other file system access	*/

	case _expfilename_:	/* expand_file_name(Short, Full) */
	    {
		extern char *expand_file();
		register PTR p = rX->v1ofcf;

		if (!IsaAtomic(p) || IsPrim(p)) {
		    ErrorMess = "! expand_file_name/2: arg 1 not an atom";
		    goto ERROR;
		}
		TRY(constarg(v2ofcf,
		    (PTR)(lookup(expand_file(AtomP(p)->stofae))) ));
	    }

	case _exists_:		/* exists(File) */
	    TRY(Exists(AtomToFile(AtomP(rX->v1ofcf))));

	case _rename_:		/* rename(OldFile, NewFile) */
	    if (rX->v2ofcf == atomnil) {
		Remove(AtomToFile(AtomP(rX->v1ofcf)));
	    } else {
		Ignore strcpy(OutBuf, AtomToFile(AtomP(rX->v1ofcf)));
		Rename(OutBuf, AtomToFile(AtomP(rX->v2ofcf)));
	    }
	    goto EXIT;

	case _chdir_:		/* cd(Dir) */
	    TRY(ChDir(AtomToFile(AtomP(rX->v1ofcf))));

	case _sh_:		/* sh */
	    TRY(!CallShell(NullS));

	case _shell_:		/* shell(_) */
	    TRY(list_to_string(rX->v1ofcf, OutBuf, 255)
		&& !CallShell(OutBuf));

	case _save_:		/* save(File, ResumeFlag) */
	    ProError("\n[ closing all files ]\n");
	    CloseFiles();
	    save();		/* will signal IO_ERROR on failure */
	    TRY(constarg(v2ofcf, ConsInt(0)));

	/*	Group 5.5: input/output flags	*/

	case _fileerrors_:	/* fileerrors */
	    fileerrors = FALSE;
	    goto EXIT;

	case _nofileerrors_:	/* nofileerrors */
	    fileerrors = TRUE;
	    goto EXIT;

	case _NOLC_:		/* 'NOLC' */
	    lc = FALSE;
	    goto EXIT;

	case _LC_:		/* 'LC' */
	    lc = TRUE;
	    goto EXIT;

	case _change_chtype_:	/* chtype(Char,Old,New) */
	    n = intval(Addr(rX->v1ofcf));
	    if ((i = GetChType(n)) < 0) goto ERROR;
	    if (!constarg(v2ofcf, ConsInt(i))) goto FAIL;
	    i = intval(Addr(rX->v3ofcf));
	    if (SetChType(n, i)) goto ERROR;
	    goto EXIT;

	case _op_:		/* $op(Prio, Type, Atom) */
	    TRY(op(rX->v1ofcf, rX->v2ofcf, Addr(rX->v3ofcf)));

	case _is_op_:		/* $is_op(+Atom, +Type, ?Prio, ?Left, ?Right) */
	    {
		int p, lp, rp;
		TRY( isop(AtomP(rX->v1ofcf),(int)XtrPosInt(rX->v2ofcf),&p,&lp,&rp)
		&& constarg(v3ofcf, ConsInt(p ))
		&& constarg(v4ofcf, ConsInt(lp))
		&& constarg(v5ofcf, ConsInt(rp))  );
	    }

	case _prompt_:		/* prompt(OldPrompt, NewPrompt) */
	    {	register PTR newprompt;
		if (!constarg(v1ofcf, atprompt)) goto FAIL;
		newprompt = vvalue(Addr(rX->v2ofcf), &k);
		if (IsPrim(newprompt) || !IsaAtomic(newprompt)) goto FAIL;
		SetPlPrompt(AtomP(newprompt)->stofae);
		atprompt = newprompt;
	    }
	    goto EXIT;


/*----------------------------------------------------------------------+
|									|
|		Group 6: other flags					|
|									|
+----------------------------------------------------------------------*/

	case _trace_:		/* trace */
	    debug = dotrace = TRUE;
	    goto EXIT;

	case _leash_:		/* $leash(Old, New) */
	    if (!constarg(v1ofcf, ConsInt(leash))) goto FAIL;
	    leash = intval(Addr(rX->v2ofcf));
	    goto EXIT;

	case _debug_:		/*  $debug(Old, New) */
	    if (!constarg(v1ofcf, ConsInt(debug))) goto FAIL;
	    debug = intval(Addr(rX->v2ofcf));
	    goto EXIT;

	case _flags_:		/*  $flags(p,old,new) */
	    {
		register byte *p = &(SkelFuncP(MolP(rX->v1ofcf)->Sk)->flgsoffe);
		if (!constarg(v2ofcf, ConsInt(*p))) goto FAIL;
		*p = (byte)intval(Addr(rX->v3ofcf));
	    }
	    goto EXIT;

	case _all_float_:	/* $all_float(Old, New) */
	    if (!constarg(v1ofcf, ConsInt(AllFloat))) goto FAIL;
	    AllFloat = intval(Addr(rX->v2ofcf));
	    goto EXIT;

	case _sysp_:		/*  $sysp(Functor,Pred#) */
	    if (rX->v2ofcf == ConsInt(0)) {
		/*  "not unknown" empty predicates  */
		SkelFuncP(MolP(rX->v1ofcf)->Sk)->moreflgs = 1;
	    } else {
		SkelFuncP(MolP(rX->v1ofcf)->Sk)->defsoffe =
		    ClauseP(XtrByte(rX->v2ofcf));
	    }
	    goto EXIT;

	case _sysflgs_:		/*  $sysflgs(Functor,Flags) */
	    SkelFuncP(MolP(rX->v1ofcf)->Sk)->flgsoffe =
		(byte)(XtrByte(rX->v2ofcf));
	    goto EXIT;

	case _recons_:		/* $recons(_) */
	    recons = rX->v1ofcf != ConsInt(0);
	    goto EXIT;

	case _carith_:		/*  $carith(Old, New) */
	    if (!constarg(v1ofcf, ConsInt(carith))) goto FAIL;
	    carith = intval(Addr(rX->v2ofcf));
	    goto EXIT;

	case _unknown_:		/*  unknown(Old, New) */
	    if (!constarg(v1ofcf, ConsInt(unknown))) goto FAIL;
	    unknown = intval(Addr(rX->v2ofcf));
	    goto EXIT;

	case _statistics_:	/*  statistics  */
	    Statistics();
	    goto EXIT;

	case _set_:		/* $SET$(ArgNo, Term, Old, New) */
	    {
		register int argno;
		register PTR term, frame;

		ErrorMess = "$SET$ botch";
		term = rX->v1ofcf;	/* arg 1 is the argument number */
		if (!IsPosInt(term)) goto ERROR;
		argno = XtrPosInt(term);
		term = rX->v2ofcf;	/* arg 2 is the term to take apart */
		if (!IsaRef(term) || IsUnbound(term)) goto ERROR;
		frame = MolP(term)->Env, term = MolP(term)->Sk;
		if (argno > SkelFuncP(term)->arityoffe) goto ERROR;
		term += argno;
		if (IsaVar(*term)) term = FrameGlo(*term, frame);
		if (Undef(frame = VarVal(term))) frame = ConsInt(0);
		else if (!IsInt(frame)) goto ERROR;
		if (!constarg(v3ofcf, frame)) goto FAIL;
		VarVal(term) = ConsInt(intval(Addr(rX->v4ofcf)));
		goto EXIT;
	    }

	case _colour_:		/* colour(C)	*/
//		Colour(intval(Addr(rX->v1ofcf)));
		goto EXIT;

	case _offset_:		/* offset(X,Y)	*/
//		Offset(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)));
		goto EXIT;

	case _enable_:		/* enable(C)	*/
//		Enable(intval(Addr(rX->v1ofcf)));
		goto EXIT;

	case _plot_:		/* plot(X,Y)	*/
		// Plot(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)));
		goto EXIT;

	case _line_:		/* line(X1,Y1,X2,Y2)	*/
		// Line(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)),
		// 			intval(Addr(rX->v3ofcf)), intval(Addr(rX->v4ofcf)));
		goto EXIT;

	case _trapeze_:		/* trapeze(X1,X2,Y1,X3,X5,Y2)	*/
		// Trapeze(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)),
		// 			intval(Addr(rX->v3ofcf)), intval(Addr(rX->v4ofcf)),
		// 			intval(Addr(rX->v5ofcf)), intval(Addr(rX->v6ofcf)));
		goto EXIT;

	case _triangle_:	/* triangle(X1,Y1,X2,Y2,X3,Y3)	*/
		// Triangle(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)),
		// 			intval(Addr(rX->v3ofcf)), intval(Addr(rX->v4ofcf)),
		// 			intval(Addr(rX->v5ofcf)), intval(Addr(rX->v6ofcf)));
		goto EXIT;

	case _fill_:		/* fill(X1,Y1,X2,Y2)	*/
		// 		Fill(intval(Addr(rX->v1ofcf)), intval(Addr(rX->v2ofcf)),
		// 			intval(Addr(rX->v3ofcf)), intval(Addr(rX->v4ofcf)));
		goto EXIT;

	case _clear_:		/* clear */
//		Clear();
//		Colour(White);
//		Enable(White);
// 		Offset(0, 0);
		MouseX = MouseY = 0;
		goto EXIT;

	case _mouse_:		/* mouse(X,Y,B)	*/
		TRY(unifyarg(Addr(rX->v1ofcf), ConsInt(MouseX), nil)
		&& unifyarg(Addr(rX->v2ofcf), ConsInt(MouseY), nil)
		&& unifyarg(Addr(rX->v3ofcf), ConsInt(MouseButtons&7), nil));

	default:
	    Ignore sprint(ErrorMess = OutBuf,
		"\n! Undefined built-in predicate : %d\n", PredNo);
	    goto ERROR;
	}
    }


    }

