/************************************************************************
*									*
*                  C Prolog    pl.h					*
*                  ========    ----					*
*									*
*  By Fernando Pereira, July 1982.					*
*  EdCAAD, Dept. of Architecture, University of Edinburgh.		*
*									*
*  Based on the Prolog system written in IMP by Luis Damas for ICL	*
*  2900 computers, with some contributions by Lawrence Byrd.		*
*									*
*  Copyright (C) 1982 Fernando Pereira, Luis Damas and Lawrence Byrd.	*
*  Beautified, made type-correct, and given additional registers in	*
*  1983 by Richard O'Keefe, DAI Edinburgh.  Made to work on GEC-63s	*
*  and other BACKWARDS machines in 1984 by the same.			*
*									*
************************************************************************/

#include <u.h>
#include <libc.h>

//#define COUNTING 1

#define PRO_X64
#define ProPlatform "x64"


#define TRUE	1
#define FALSE	0

#define Ignore (void)	/* make Lint shut up */

/* the representable integers are in the interval [MinInt,MaxInt] */

#define MaxInt	268435455	/* largest integer = 2**28-1 */
#define MinInt	-268435456	/* smallest integer = -2**28 */


/*  Signed() must be defined as the type of integers with the same width
    as  a  pointer  (int  on  a  VAX).   This  type is used to interpret
    pointers as signed numbers in comparisons related  to  the  internal
    representation  of  terms  (see picture below).  Despite attempts to
    parameterise as much  as  possible,  the  system  still  depends  on
    pointers   being   32   bits   wide   and   numbers  being  held  in
    two's-complement.  The original EMAS Prolog in IMP relies  too  much
    on  pointers being just addresses, independently of the type pointed
    to, for me to be able to do anything about it.  To help satisfy  the
    compiler's  view of typing, addresses are objects of type PTR, which
    are then cast into the appopriate types.
*/

typedef vlong ProInt;
typedef uvlong ProUInt;
typedef vlong ProLong;
typedef uvlong ProULong;

typedef struct Value {
	union {ProLong asInt; double asFloat;} val;
	char Float;
} Value;

typedef uvlong **PTR;	/* anonymous pointers */
typedef uchar byte;	/* used for small counts */

#define ProVarNumberMask 0xffffffffffff

/*  Typed pointers  */

typedef	struct	ATOM		*ATOMP;
typedef	struct	CLAUSEREC	*CLAUSEP;
typedef	struct	FRAME		*FRAMEP;
typedef	struct	FUNCTOR		*FUNCTORP;
typedef	struct	MOL			*MOLP;
typedef	struct	SKEL		*SKELP;

/*  Pointer casts  */

#define CellP(p)	((PTR*)(p))
#define AtomP(p)	((ATOMP)(p))
#define FunctorP(p)	((FUNCTORP)(p))
#define FrameP(p)	((FRAMEP)(p))
#define ClauseP(p)	((CLAUSEP)(p))
#define MolP(p)		((MOLP)(p))
#define SkelP(p)	((SKELP)(p))
#define Signed(p)	((ProInt)(p))
#define Unsigned(p)	((ProUInt)(p))
#define CharP(p)	((char*)(p))
#define SkelFuncP(t)	(SkelP(t)->Fn)
#define SkelAtomP(t)	(SkelP(t)->Fn->atoffe)

#define	NullC		((CLAUSEP)0)
#define NullF		((FILE*)0)
#define	NullP		((PTR)0)
#define	NullS		((char*)0)

#define Addr(p)		((PTR)&(p))	/* PTRise an address */

#define NextArg(p)	(*(PTR*)++p)

/* Words(number) converts a number of bytes to a number of PTRs */

#define Words(v) (((v)+sizeof(PTR)-1)/sizeof(PTR))

/*----------------------------------------------------------------------

    The definitions that follow depend crucially on  the  relative
    positions  of stacks and other work areas.  Values of type PTR
    whose most significant bit is 1 (negative as integers) are not
    pointers but values of some primitive type (integers,  floats,
    or  database  pointers).   Positive PTRs are pointers into the
    stacks/work areas.  Therefore, the top  of  the  topmost  area
    must  be below address 2^30-1.  If Unix could cope with widely
    scattered segments (promised for 4.2 BSD), I might be able  to
    implement   these  things  more  nicely  in  terms  of  tagged
    pointers.   As  it  is,  type  checking  is  done  by   signed
    comparisons,  and  the  stacks/work  areas occupy a contiguous
    region in memory.  This region is allocated by  CreateStacks()
    (sysbits.c), and area sizes are defined in parms.c.

    The layout is as follows:
	-2^31:		+------------------------+
			|integers, floats, dbrefs|
	0:		+------------------------+
			:			 :
	auxstk0:	+------------------------+
			|    Auxiliary stack	 |
	tr0:		+------------------------+
			|    Trail		 |
	atom0:  	+------------------------+
			|    Atoms		 |
	heap0:		+------------------------+
			|    Heap (input terms)	 |
	glb0:		+------------------------+
			|    Global stack	 |
	lcl0:		+------------------------+
			|    Local stack	 |
			+------------------------+

    Primitive objects (ints, floats and database  pointers  at  present)
    are  distinguished  by tag bits.  Any primitive object will have bit
    31 (sign bit) set.  The tags are as follows:

			      tag bits
			      31 30 29
		=============+==+==+==
		int           1  1  1
		float         1  1  0
		ptr to clause 1  0  0
		ptr to record 1  0  1

    It turns out that the GEC series 63 has  every  bit  as  lunatic  an
    operating  system as PR1ME computers (PR1ME put software last!), and
    it wants to put users into negative address space.  (Can this be  an
    indication of GEC's regard for users?)  There may be other operating
    systems  with  the  same  stupid  notion,  so  you can now configure
    C-Prolog for them by defining BACKWARDS.  On  a  BACKWARDS  OS,  the
    partitions are

	auxstk0:	+------------------------+
			|    Auxiliary stack	 |
	tr0:		+------------------------+
			|    Trail		 |
	glb0:		+------------------------+
			|    Global stack	 |
	lcl0:		+------------------------+
			|    Local stack	 |
	heap0:		+------------------------+
			|    Heap (input terms)	 |
	atom0:  	+------------------------+
			|    Atoms		 |
			+------------------------+
			:			 :
	0:		+------------------------+
			|integers, floats, dbrefs|
	2^31-1:		+------------------------+

    The difference is that the heap and the atoms have swappwed places and
    moved above the stacks.  In both normal and BACKWARDS machines, the C
    initialised data is assumed to be above auxstk0 in the diagram, and in
    both the memory partitions all grow in the positive direction.  (If it
    is possible for auxstk0 to start at 0x80000000 you are still safe, as
    GLOVAR0 and LCLVAR0 will only put you crook if heap0-0x80000000 is less
    than 2^17 words, and you can't run C-Prolog with stacks that tiny.)

    Primitive objects (ints, floats and database  pointers  at  present)
    are  distinguished  by tag bits.  Any primitive object will have bit
    31 (sign bit) OFF.  The tags are as follows:

			      tag bits
			      31 30 29
		=============+==+==+==
		int           0  0  0
		float         0  0  1
		ptr to clause 0  1  0
		ptr to record 0  1  1

----------------------------------------------------------------------*/

/* Signed pointer comparison -- BEWARE */

#define	SC(x,r,y) (Signed(x) r Signed(y))

/* Mask comparison, yes I know about the apparently signed constants */

#define	UM(x,m,v) ((Unsigned(x)&Unsigned(m)) == Unsigned(v))

/*  The following change has been made for 1.2D.edai.  We  want gunify()
    to  be  blindingly  fast.  Therefore, we want to make as many of the
    tests as  possible  fast.   The  VAX  C  compiler  lets  us  have  6
    registers,  and I was only using 4.  What I have done, therefore, is
    to put two of the boundaries into registers, and have created  local
    definitions of these tests.  Only tests with real programs will tell
    us whether that is a worthwhile change.  IsVar and Undef are already
    fast, they need no extra registers.  The result is a 10-20% speedup!
    It finally dawned on me that these values can remain in registers in
    the whole of main().  If only we could reserve these registers!   On
    the Orion the result is only a 2% speedup, and a 224 byte saving,but
    it does no harm, so...  Normal machines put glb0 and heap0 into fast
    registers, BACKWARDS machines put atom0 and heap0.   It is good that
    heap0 is in a register either way, for XtraDB and ConsaDB.  Lucky!
*/

#ifndef	USEREGS
#   define	atomREG atom0
#   define	heapREG	heap0
#   define	glbREG  glb0
#   define	DeclRegisters
#   define	InitRegisters
#else	USEREGS
#ifdef BACKWARDS
#   define	DeclRegisters	register PTR heapREG, atomREG;
#   define	InitRegisters	heapREG = heap0, atomREG = atom0;
#else
#   define	DeclRegisters	register PTR glbREG, heapREG;
#   define	InitRegisters	glbREG = glb0, heapREG = heap0;
#endif	BACKWARDS
#endif	USEREGS

/*----------------------------------------------------------------------+
|									|
|    VARIABLES IN THE STACKS						|
|	The relative order of global and local variables is the same	|
|	whether the OS is BACKWARDS or not.  We use ordinary pointer	|
|	comparisons throughout the whole of C Prolog for comparing a	|
|	pair of variables, this had better be unsigned comparison. A	|
|	variable whose value is 0 (which is otherwise meaningless in	|
|	C-Prolog) is understood to be unbound.				|
|									|
+----------------------------------------------------------------------*/

#ifdef BACKWARDS
#   define IsRef(c)	SC(c,<,heap0)	/* Prolog variable */
#   define IsaRef(c)	SC(c,<,heapREG)
#else  !BACKWARDS
#   define IsRef(c)		SC(c,>=,glb0)	/* Prolog variable */
#   define IsaRef(c)	SC(c,>=,glbREG)
#endif	BACKWARDS

#define VarVal(p)		(*(PTR*)(p))		/* variable's binding */
#define Undef(c)		((c) == NullP)		/* undefined value */
#define	IsUnbound(c)	(VarVal(c) == NullP)	/* unbound variable */

/* Grow stacks. */

#define GrowLocal(n)	(Regs_V = FrameP(Regs_v+(n)))

#define GrowGlobal(n)	(Regs_v1 += (n))

#define InitGlobal(n,k)	{   register PTR p = Regs_v1; \
			    k = p, p += n, Regs_v1 = p; \
			    while (p > k) *CellP(--p) = NullP;	}

/* construct molecule */

#define	ConsaMol(s,e,t,v) t=Regs_v1,Regs_v1+=MolSz,MolP(t)->Sk=s,MolP(t)->Env=e,VarVal(v)=t
#define ConsMol(s,e,m) MolP(Regs_v1)->Sk=(s),MolP(Regs_v1)->Env=(e),m=Regs_v1,Regs_v1+=MolSz

/* trail an assignment */

/*  TrailGlo(r) is to be used when r is a register and is known to hold a
    global variable.  TrailReg(r) is to be used when r is a register, but
    it is not known whether it holds a local or a global variable.   When
    t is a general expression, use TrailVar instead, where
    define TrailVar(t)	{ PTR tt = (t); \
			  if (tt < vv1 || (lcl0 <= tt && tt < vv)) TrailPtr(tt)}
*/

#define TrailPtr(t)	{if (Regs_tr > trmax) NoSpace(TrailId); *(PTR *)Regs_tr++ = (t);}
#define TrailGlo(r)	{if (r < Regs_vv1) TrailPtr(r)}
#define TrailReg(r)	{if (r < Regs_vv1 || (r >= lcl0 && r < Regs_vv)) TrailPtr(r)}


/*----------------------------------------------------------------------+
|									|
|   VARIABLES IN SKELETONS						|
|	are represented as Base+Offset, where Base is GLOVAR0 for a	|
|	global variable or LCLVAR0 for a local variable, and Offset	|
|	is the amount to be added to a global frame pointer (i.e. a	|
|	value of V1) or a local frame pointer (i.e. a value of V,X)	|
|	to get a machine pointer to a runtime variable.   Note that	|
|	the offset is NOT a variable number.  For a global variable	|
|	it is a variable number multiplied by the number of address	|
|	units per word (4 on a VAX, 2 on a Perq, 1 on an Orion) and	|
|	for a local variable it is that plus a further offset which	|
|	results from the fact that local frames contain more things	|
|	than just variables.   It is the task of the SkelGlobal and	|
|	SkelLocal macros to do the conversion (which doesn't happen	|
|	very often, so that FrameVar and FrameGlo can use unscaled&	|
|	unoffset integer addition (and they happen a lot).  GLOVAR0	|
|	and LCLVAR0 have to look like IsRefs so that they aren't at	|
|	all like anything else that can be in a skeleton.  Any such	|
|	value will do, as real IsRefs can't occur in clauses.  Like	|
|	TrailGlo -vs- TrailVar, there are two macros for handling a	|
|	skeleton variable: FrameVar(var, glofr, lclfr) takes a var,	|
|	a global frame pointer, and a local frame pointer, and uses	|
|	whichever frame the variable wants, FrameGlo knows that the	|
|	variable is global so just takes a global frame pointer.	|
|									|
+----------------------------------------------------------------------*/

#ifdef BACKWARDS
#   define GLOVAR0	0x80000000	/* Offset for global variables */
#   define LCLVAR0	0x80010000	/* Offset for local  variables */
#   define IsVar(c)	(Signed(c) < 0x80020000)
#else  !BACKWARDS

#ifdef PRO_X64
	#define GLOVAR0	0x7ffe000000000000	/* Offset for global variables */  // 0x7ffe0000
	#define LCLVAR0	0x7fff000000000000	/* Offset for local  variables */  // 0x7fff0000
#else
	#define GLOVAR0	0x7ffe0000	/* Offset for global variables */
	#define LCLVAR0	0x7fff0000	/* Offset for local  variables */
#endif
#define IsVar(c)	(Signed(c) >= GLOVAR0)

#endif	BACKWARDS

#define	IsaVar(c)	IsaRef(c)	/* Fast "hack" version of IsVar */

/*  Take a frame pointer (local or global) and a variable descriptor
    from a skeleton, and construct a C pointer into the appropriate
    stack.  Note that SkelLocal takes care of the offset for locals.
    FrameVar is the only thing that cares whether a skeleton variable
    is local or global, everything else trusts it or knows it's global.
*/
#ifdef PRO_X64
	#define FrameGlo(V,G)	(PTR)(Unsigned(G)+(Unsigned(V)&0xffffffffffff))
#else
	#define FrameGlo(V,G)	(PTR)(Unsigned(G)+(Unsigned(V)&0xffff))
#endif
#define	FrameVar(V,G,L)	FrameGlo(V,Signed(V) >= LCLVAR0 ? L : G)

/* skeleton representation of the nth local variable */

#define SkelLocal(n) ((PTR)((Addr(FrameP(LCLVAR0)->v1ofcf)+(n))))

/* ditto global variable */

#define SkelGlobal(n) (((PTR)GLOVAR0)+(n))

/*----------------------------------------------------------------------+
|									|
|   ATOMIC OBJECTS (ATOMS AND PRIMITIVES)				|
|	Atomic includes atoms, integers, floats, and data base refs.	|
|	By virtue of including the value 0 in its range it also can	|
|	include unbound variables' values, which we use on occasion.	|
|	We even use the fact that it includes the range 0..255 (the	|
|	range of primitive predicates) to avoid putting proper tags	|
|	on these values.  (The trick works on BACKWARDS machines as	|
|	well.  The IsAtomic test is made a lot, so it helps to have	|
|	the boundary in a register on most machines.  Note that the	|
|	boundary is *different* on BACKWARDS machines.			|
|	There are four main ranges:					|
|		global and local variables: IsRef(c)			|
|		skeletons & clauses: !IsRef(c) && !IsAtomic(c)		|
|		atoms: IsAtomic(c) && !IsPrim(c)			|
|		integers, floats, dbrefs: IsPrim(c)			|
|	There are other things in the address space, but Prolog has	|
|	no occasion to point to them (trail pointers in frames apart).	|
|									|
+----------------------------------------------------------------------*/

#ifdef BACKWARDS
#   define IsPrim(c)	SC(c,>=,0)	/* number or db reference */
#   define IsAtomic(c)	SC(c,>=,atom0)	/* atomic term */
#   define IsaAtomic(c)	SC(c,>=,atomREG)
#else  !BACKWARDS
#   define IsPrim(c)	SC(c,<,0)
#   define IsAtomic(c)	SC(c,<,heap0)
#   define IsaAtomic(c)	SC(c,<,heapREG)
#endif	BACKWARDS
#define IsAtom(c)	(!IsPrim(c) && IsAtomic(c))

/*----------------------------------------------------------------------+
|									|
|   INTEGERS.								|
|	The coding is such that the bottom 29 bits of the integer are	|
|	already in twos-complement form.  If << and >> are arithmetic	|
|	shifts it doesn't matter what the tag is.  This seems to work	|
|	on the VAX, Perq, and Orion, and is true on the PDP-11.  When	|
|	this doesn't work the alternative is quite a bit slower.  Two	|
|	special cases are handled: XtrByte is used when you know that	|
|	the integer lies between 0 and 255, or when you only want the	|
|	bottom 8 bits.  XtrPosInt is used when you know the integer's	|
|	positive.  Note that INT0 and INT1 are just masks.		|
|									|
+----------------------------------------------------------------------*/

#ifdef PRO_X64
	#define SIGN		0x1000000000000000	/* sign bit on a constructed number */
	#define INT0		0xe000000000000000	/* mask for primitive tag bits */
	#define INT1		0xf000000000000000	/* combines INT0 and SIGN */
#else
	#define SIGN		0x10000000	/* sign bit on a constructed number */
	#define INT0		0xe0000000	/* mask for primitive tag bits */
	#define INT1		0xf0000000	/* combines INT0 and SIGN */
#endif

#ifdef BACKWARDS
#   define IsInt(c)	UM(c,INT0,0)
#   define IsPosInt(c)	UM(c,INT1,0)
#   define IsByteInt(c) UM(c,~255,0)
#   define ConsInt(i)	((PTR)((i) &~ INT0))
#else  !BACKWARDS
#   define IsInt(c)	UM(c,INT0,INT0)
#   define IsPosInt(c)	UM(c,INT1,INT0)
#   define IsByteInt(c)	(Signed(c) >= INT0 && Signed(c) <= (INT0|0xFF))
#   define ConsInt(i)	((PTR)(INT0|(i)))
#endif	BACKWARDS

#define XtrByte(c)	(Signed(c) & 0xFF)
#define XtrPosInt(c)	(Signed(c) &~ INT1)
#define	XtrInt(c)	((Signed(c)<<3)>>3)

/*  When >> is a logical shift, use
    define XtrInt(c)	(Signed(c)&SIGN ? Signed(c) &~ INT0 : Signed(c) | INT0)
*/

/*----------------------------------------------------------------------+
|									|
|   FLOATING-POINT NUMBERS.						|
|	The bulk of the work of encoding and decoding floats left to	|
|	the file arith.c.  One problem is that we need to take three	|
|	least significant bits away, and their location varies among	|
|	machines.   Arith.c knows about the IEEE and VAX-11 formats.	|
|	IsNumber(c) recognises any sort of number.  C-Prolog doesn't	|
|	offer a float(X) predicate because it will store a number as	|
|	an integer if it possibly can.					|
|									|
+----------------------------------------------------------------------*/

#ifdef BACKWARDS
#   define FRM0		0x00000000	/* tag for frame information word */
#   define FLT0		0x20000000	/* floating-point tag */
#else  !BACKWARDS

#ifdef PRO_X64
	#define FRM0		0xc000000000000000	/* tag for frame information word */
	#define FLT0		0xc000000000000000	/* floating-point tag */
	#define FLT1		0xc000000000000000	/* primitive? number? */
#else
	#define FRM0		0xc0000000	/* tag for frame information word */
	#define FLT0		0xc0000000	/* floating-point tag */
	#define FLT1		0xc0000000	/* primitive? number? */
#endif

#endif	BACKWARDS

#define	IsFloat(c)	UM(c,INT0,FLT0)	/* is a floating point number */
#define	IsNumber(c)	UM(c,FLT1,FLT0&FLT1)

extern float XtrFloat();
extern PTR ConsFloat();

/*----------------------------------------------------------------------+
|									|
|   DATA BASE REFERENCES.						|
|	Data base references are basically just pointers to clauses or	|
|	to records.  I am not at all sure why Fernando decided to tell	|
|	them apart by tagging the pointers, we could just as well flag	|
|	the clauses or records themselves.  (There are enough bits and	|
|	to spare in infofcl.)  If the heap were under the atom area we	|
|	we could then use the pointers themselves as primitives.  What	|
|	happens now is that the word offset of a clause or record from	|
|	the start of the heap is taken as an integer value and tagged.	|
|	It is worth spending some effort on the coding as DBreferences	|
|	are pushed onto the trail fairly often so that retracted stuff	|
|	won't vanish while it's still in use.				|
|									|
+----------------------------------------------------------------------*/

#ifdef BACKWARDS
#   define IsDBRef(c)	SC(c,>=,REF0)	/* clause or record pointer */
#   define REF0		0x40000000	/* origin of data base references */
#	define RECORD		0x20000000	/* reference to a record */
#	define CLAUSE		0L		/* reference to a clause (clear) */
#else  !BACKWARDS
#   define IsDBRef(c)	SC(c,<,FLT0)	/* clause or record pointer */
#ifdef PRO_X64
	#define REF0		0x8000000000000000	/* origin of data base references */
	#define RECORD		0x2000000000000000	/* reference to a record */
	#define CLAUSE		0x0000000000000000	/* reference to a clause (clear) */
#else
	#define REF0		0x80000000	/* origin of data base references */
	#define RECORD		0x20000000	/* reference to a record */
	#define CLAUSE		0L		/* reference to a clause (clear) */
#endif

#endif	BACKWARDS

#define XtrDBRef(c)	((CLAUSEP)(heap0+(Unsigned(c)&~INT0)))
#define ConsDBRef(p,k)	((PTR)(((PTR)(p)-heap0)|(REF0|(k))))
#define IsClause(c)	!(Unsigned(c)&RECORD)

/*  Fast versions  */
#define XtraDB(c)	((CLAUSEP)(heapREG+(Unsigned(c)&~INT0)))
#define ConsaDB(p,k)	((PTR)(((PTR)(p)-heapREG)|(REF0|(k))))

/*  The following two macros may or may not be replaced by procedures, as has
    been done in 1.4.sri.  They are only used in auxfn.c and dbase.c.
*/

#define	Unsafe()	crit=1
#define Safe()		if (crit==2) crit=0,Event(ABORT); crit=0

/*  The hash table is fairly crude.  A  very  simple  hash  function  is
    calculated,  and used to index an array of lists of atoms.  HashSize
    is the number of lists.  It should be at least 128.  The size has to
    be a power of 2 so that X&(HashSize-1) and X%HashSize are the same.
    The hash function is the best of those I've tested, see hash-test.c.
*/
#define	HashSize	256

typedef struct ATOM			/* atom block */
    {
	ATOMP	 atofae;		/* self pointer */
	byte	 arityofae;		/* arity ( = 0 ) */
	byte	 flgsofae;		/* flags field */
	short	 infxofae;		/* infix priority */
	CLAUSEP	 defsofae;		/* chain of definitions */
	CLAUSEP	 dbofae;		/* chain of records */
	FUNCTORP fcofae;		/* functor chain */
	ATOMP	 nxtofae;		/* hash chain */
	short	 prfxofae;		/* priority as prefix op */
	short	 psfxofae;		/* priority as postfix op */
	char	 stofae[1];		/* string for atom */
    }	ATOM;
					/* size of atom block (excluding name) */
#define szofae sizeof(ATOM)


typedef struct FUNCTOR			/* functor block */
    {
	ATOMP	 atoffe;		/* atom for this functor */
	byte	 arityoffe;		/* arity */
	byte	 flgsoffe;		/* flags field */
	short	 moreflgs;		/* to be used by arithmetic operators */
	CLAUSEP	 defsoffe;		/* clauses for this functor */
	CLAUSEP  dboffe;		/* data base entries under functor */
	FUNCTORP nxtoffe;		/* chain of functors with same name */
	PTR	 gtoffe;		/* start of general skeleton for this */
    }	FUNCTOR;			/* term. points to this entry. */
					/* size of functor entry (+ arity) in PTRs */
#define szoffe	(sizeof(FUNCTOR)/sizeof(PTR))


#define	MaxArity 200			/* maximum arity of any functor */
#define	MaxVar	 255    		/* highest possible variable number */

typedef struct CLAUSEREC		/* clause or record block */
    {
	byte	 infofcl;		/* general inf. */
	byte	 ltofcl;		/* no. of local and temp. variables */
	byte     lvofcl;		/* no. of local vars */
	byte     gvofcl;		/* no. of global vars */
	ProLong	 refcofcl;		/* reference count */
	PTR	 hdofcl;		/* clause head */
	PTR	 bdyofcl;		/* body of clause */
	CLAUSEP	 altofcl;		/* alternatives */
	CLAUSEP	 prevofcl;		/* previous clause */
    }	CLAUSEREC;
					/* size of clause block in PTRs */
#define szofcl	(sizeof(CLAUSEREC)/sizeof(PTR))


typedef struct FRAME			/*  local frame  */
    {
	PTR	 gofcf;			/* goal */
	FRAMEP	 gfofcf;		/* goal's local frame */
	FRAMEP	 lcpofcf;		/* previous choice point */
	PTR	 gsofcf;		/* global frame associated with this frame */
	PTR	 trofcf;		/* tr at entry */
	ProLong	 infofcf;		/* general information */
	PTR	 cofcf;			/* continuation on entry */
	CLAUSEP* altofcf;		/* alternatives */
    /*--------------------------------------------------------*/
	PTR	 v1ofcf;		/* variables */
	PTR	 v2ofcf;		/* if more variables are added, */
	PTR	 v3ofcf;		/* adjust the constant 6 in the */
	PTR	 v4ofcf;		/* definition of szofcf */
	PTR	 v5ofcf;
	PTR	 v6ofcf;
    }	FRAME;

					/* frame size in PTRs */
#define szofcf	(sizeof(FRAME)/sizeof(PTR)-6)
					/* greatest possible frame size */
#define	MaxFrame (szofcf+MaxVar+1)

/*  The information word in a local frame holds the call number and the
    call depth.  It is made to look like a Prolog primitive, so that it
    is handled properly by restore().   The tags originally used looked
    like clause or record pointers.  They are now like numbers.  We can
    can tell a system (hidden) frame from an ordinary frame by checking
    the invocation number field: it is 0 for the hidden frames only. If
    the invocation number reaches 2^18 we will of course miss that goal
    but the tag will still be safe, and the count will wrap round.
*/
#define	LEVEL		0xfff	/* recursion level mask */
#define LEVEL_WIDTH	12	/* width of the level field */
#define CALL_NUMBER	0x3ffff	/* invocation number mask */
#define CALL_WIDTH	18	/* width of the call number field */
#define IsVisible(info)	(info&(CALL_NUMBER<<LEVEL_WIDTH))

/*  Prolog frame pointer registers as PTRs  */

#define Regs_x		(PTR) Regs_X
#define Regs_v		(PTR) Regs_V
#define	Regs_vv		(PTR) Regs_VV

/*  skeleton layout - add argument fields as required by code
    (could define an arg. array instead, the generality is hardly worth
    the bother)
*/

typedef struct SKEL
    {
	FUNCTORP Fn;
	PTR	 Arg1;
	PTR	 Arg2;
    }	SKEL;

#define SkelSz(n)	((n)+1)	/* size of n arg. skeleton */


/* molecule layout - the Sk field MUST be the first! */

typedef struct MOL
    {
	PTR Sk;		/* normally SKELP, but can be ATOMP */
	PTR Env;
    } MOL;

#define MolSz	2	/* NB: size in PTRs, not in bytes */

/* clause/record flags */

enum { ERASED = 1,		/* erased but not yet removed */
		IN_USE };		/* in use in a proof, cannot be deleted yet */

/*  Flags in a functor entry.  They apply to the predicate.  NB: the low
    order  4  bits  of  the  flags  field in a functor or atom entry, if
    non-zero, represent the internal code of that functor or atom as  an
    arithmetic   operator.   This  means  that  the  maximum  number  of
    different arithmetic operators for each arity is 15  (see  arith.c).
    This  could  be changed by redefining the layout of functor and atom
    entries,  taking  care  with  the  way  unions  are  used  in   them
    (left-overs from the original code in IMP).  There is a spare 16-bit
    slot  in  functor  entries,  so  any  reasonable number of unary and
    binary functions could be handled.  Whether-to-float could  also  go
    there.
*/

#define RESERVED	0xe0	/* cannot be modified */
#define HIDDEN		0xc0	/* Protected or Transparent */
#define PROTECTED	0x80	/* cannot be listed, traced, or modified */
#define INVISIBLE	0x40	/* invisible to tracing and recursion level */
#define UNTRACEABLE	0x20	/* cannot be traced */
#define SPY_ME		0x10	/* is a spypoint */


/* Internal event conditions */

enum { COLD_START,		/* first thing in the morning */
		ABORT,			/* 'abort' evaluable predicate */
		IO_ERROR,		/* every STDIO failure that is not */
		END_OF_FILE,	/* end of file on input */
		ARITH_ERROR,	/* incorrect args. to 'is", =:=, etc. */
		GEN_ERROR };	/* all others report here */

/* Initial state flags (set by switches: see parms.c) */

#define	InBoot	 State[IN_BOOT]
#define	Announce State[TRACE]

enum { IN_BOOT, DEBUG, QUIET, TRACE };

/* predefined I/O streams */
enum { STDIN, STDOUT, STDERR };

/* Work areas */

/* Indices of areas */

#ifdef BACKWARDS

enum { AuxId, TrailId, GlobalId, LocalId, HeapId, AtomId };

#else  !BACKWARDS

enum { AuxId, TrailId, AtomId, HeapId, GlobalId, LocalId };

#endif	BACKWARDS
#define NAreas		6	/* number of work areas/stacks */

/* origins of work areas */

#define auxstk0	Origin[AuxId]
#define tr0		Origin[TrailId]
#define atom0	Origin[AtomId]
#define heap0	Origin[HeapId]
#define glb0	Origin[GlobalId]
#define lcl0	Origin[LocalId]

/* limits of work areas */

#define auxmax	Limit[AuxId]
#define trmax	Limit[TrailId]
#define atmax	Limit[AtomId]
#define hpmax	Limit[HeapId]
#define v1max	Limit[GlobalId]
#define vmax	Limit[LocalId]

/* initial atoms */

#define	atomnil		(PTR)nilatom
#define nilatom		BasicAtom[0]		/* []		*/
#define commaatom	BasicAtom[1]		/* ,		*/
#define bracesatom	BasicAtom[2]		/* {}		*/
#define LessThan	BasicAtom[3]		/* <		*/
#define Equal		BasicAtom[4]		/* =		*/
#define GreaterThan	BasicAtom[5]		/* >		*/
#define Minus		BasicAtom[6]		/* -		*/
#define semicatom	BasicAtom[7]		/* ;		*/
/*  The remaining atoms are only used in main.c, as terms.	*/
/*  'user' is also used in sysbits.c, but has to be a term too.	*/
#define EndOfFile  (PTR)BasicAtom[8]		/* end_of_file	*/
#define atomtrue   (PTR)BasicAtom[9]		/* true		*/
#define user	   (PTR)BasicAtom[10]		/* user		*/
#define	useratom	BasicAtom[10]		/* user		*/
#define Yes	        BasicAtom[11]		/* $yes		*/
#define No	        BasicAtom[12]		/* $no		*/
#define live	   (PTR)BasicAtom[13]		/* $live	*/
#define breakat    (PTR)BasicAtom[14]		/* $break	*/

/* initial functors */

#define calltag		BasicFunctor[0]		/* call(G)	*/
#define commatag	BasicFunctor[1]		/* A,B		*/
#define assertfunc	BasicFunctor[2]		/* {G}		*/
#define listfunc	BasicFunctor[3]		/* [H|T]	*/
#define arrowtag	BasicFunctor[4]		/* H:-B		*/
#define provefunc	BasicFunctor[5]		/* :-Q		*/
#define HiddenCall	BasicFunctor[6]		/* $hidden_call	*/

/* Number of pre-defined atoms and functors -- these must appear at the
   start of any boot file.
*/

#define rqrdatoms 15

#define rqrdfuncs 7

/* end of file character -- ^Z to simulate DEC-10 Prolog */

#define	CtrlZ	'\032'

/* public data */

extern ATOMP
	BasicAtom[], FileAtom[];

extern FUNCTORP
	 BasicFunctor[];

extern FRAMEP
    Regs_X, Regs_V, Regs_VV;

extern PTR
	Limit[], Origin[],
	Regs_x1, Regs_v1, Regs_vv1, Regs_tr, fp, hasha, list10, vra, vrz;

extern char
	*AreaName[], *ErrorMess, OutBuf[], PlPrompt[], savemagic[];

extern ProLong
	Size[];

extern int
	AllFloat, Input, Output, State[],
	crit, debug, dotrace, errno, lc, quoteia, running, saveversion, sklev;

/* public functions */

/* arith.c */
void ArithError(char *s);
void NotInt(FUNCTORP fn);
void NotOp(FUNCTORP fn);
double ffail(double d);
int Narrow(double f, ProLong *i);
int ForceInt(register Value *val);
void eval(register PTR t, PTR frame);
ProLong intval(PTR p);
PTR numeval(PTR p);
int numcompare(int op, PTR t1, PTR t2);
PTR ConsFloat(double f);
float XtrFloat(PTR p);

/* auxfn.c */
ATOMP lookup(char *id);
FUNCTORP fentry(ATOMP atom, int arity);
PTR apply(ATOMP atom, register int arity, register PTR *args);
PTR makelist(register ProLong n, register PTR *elements);
int list_to_string(register PTR list, register char *s, int n);
int clause_number(FRAMEP frame, PTR goal);
void backtrace(void);
void Statistics(void);

/* compare.c */
ProLong comp(register PTR T1, PTR E1, register PTR T2, PTR E2);
ProLong icompare(void);
ATOMP acompare(void);
ATOMP kcompare(register PTR T1, PTR E1, register PTR T2, PTR E2, register int n);

/* dbase.c */
void dispose(register CLAUSEP clause);
void freeskel(register PTR term);
void unchain(PTR ref);
void hide(PTR ref);
int erased(PTR ref);
int erase(register PTR ref);
PTR termtoheap(register PTR t, PTR frame);
PTR bodytoheap(register PTR t, PTR frame);
void scan(PTR *c);
void abolish(register FUNCTORP fn, int UserCall);
PTR record(ProLong key, PTR t, PTR rk, ProInt aorz);
PTR recorded(ProLong key);
PTR globalise(register PTR t, ProInt bodyflg);
int instance(PTR ref, PTR argp);

/* info.c */
void ProPrintAtom(char *a_buf, ATOMP a_atom);
void ProPrintFunctor(char *a_buf, FUNCTORP a_functor);
void ProPrintObj(char *a_buf, PTR a_obj, PTR v1t, PTR vt, char a_adr);
void ProPrintObjList(char *a_buf, PTR a_obj, PTR v1t, PTR vt, SKELP a_list, char a_adr);
void ProPrintSkel(char *a_buf, SKELP a_skel, PTR v1t, PTR vt, char a_adr);
void ProPrintSkelList(char *a_buf, SKELP a_skel, PTR v1t, PTR vt, SKELP a_list, char a_adr);
void ProPrintClause(char *a_buf, CLAUSEP a_clause, PTR v1t, PTR vt, char a_adr);
void ProShowSkel(SKELP a_skel, PTR v1t, PTR vt, char a_adr);
void ProShowClause(CLAUSEP a_clause, PTR v1t, PTR vt, char a_adr);
void ProCheckClause(CLAUSEP a_clause, PTR v1t, PTR vt, char *a_text);
int ProCheckSkelTry(SKELP a_skel, const char *a_text);
int ProCheckClauseTry(CLAUSEP a_clause, const char *a_text);

/* main.c */
void savev(register PTR *p, register int n);
void restv(register PTR *p, register int n);
void savevars(void);
void restvars(void);
void save(void);
void remap(register PTR *tp);
int restore(char *sfile);
void ResetTrail(void);
PTR bread(void);
void Halt(int why);

/* parms.c */
int numval(char **q);
int findch(register int c, register char *s);
char *crack(int argc, char **argv);

/* rewrite.c */
int NumberString(char **s, PTR *p, int free);
int GetChType(register int ch);
int SetChType(register int ch, register int type);
int isop(ATOMP atom, int optype, int *p, int *lp, int *rp);
int maxprio(register ATOMP atom, register int priority);
int op(PTR prio, PTR optype, register PTR spec);
void PutAtom(ATOMP at);
void spaceout(ATOMP a);
char *num2chars(PTR k);
void ProPWrite(register PTR term, PTR frame, register int priority);
PTR lookupvar(char *id);
void SyntaxError(void);
int token(void);
PTR readargs(ATOMP atom);
PTR stringtolist(void);
PTR readlist(void);
int prefix_is_atom(int n, int m);
PTR term(register int n);
PTR ProPRead(PTR *names);

/* space.c */
void InitHeap(void);
void release(PTR Base, register int size);
void CollectGarbage(void);
PTR getsp(register int size);
void RelocHeap(register ProLong delta);
ProLong HeapUsed(void);
PTR HeapTop(void);

/* sysbits.c */
const char *SysError(void);
void Stop(int dump);
void Event(int n);
void NoSpace(int s);
void TakeSignal(int s);
void CatchSignals(void);
void SyntErrPos(void);
char *CopyPrefix(register char *from, register char *to);
char *expandfile(register char *Fancy);
void InitIO(void);
int ProMessage(const char *fmt, ...);
int ProError(const char *fmt, ...);
void LockChannels(int lock);
void CClose(register int i);
void PClose(ATOMP file);
void CloseFiles(void);
void Seen(void);
void Told(void);
ATOMP Seeing(void);
ATOMP Telling(void);
char *AtomToFile(register ATOMP file);
int COpen(char *title, int mode);
void CSee(char *title);
void Flush(register PTR file);
void See(register ATOMP file);
void Tell(register ATOMP file, int append);
void Put(int c);
void PutString(register char *s);
void SetPlPrompt(char *s);
void Prompt(char *s);
void PromptIfUser(char *s);
int Get(void);
int ToEOL(void);
int CurLineNo(register ATOMP file);
void Interrupt(void);
int Exists(char *title);
void Rename(char *oldname, char *newname);
void Remove(char *title);
int ChDir(char *newdir);
int CallShell(char *command);
void CreateStacks(void);
double CpuTime(void);

/* unify.c */
int gunify(PTR ta, PTR ga, PTR tb, PTR gb);
int unifyarg(register PTR arg, register PTR term, PTR frame);
PTR vvalue(register PTR var, PTR *framev);
PTR arg(register PTR a, PTR frame);
PTR argv(register PTR a, PTR frame, PTR *af);
