/************************************************************************
*									*
*		   C Prolog	space.c					*
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
*  This module is based on published work by Weinberg? Weisberg?	*
*									*
************************************************************************/

#include "pl.h"

#if	ERRCHECK
 int ReleaseCheck = 1;
 int clock = 0;
#endif	ERRCHECK

#define	CHAINS		16
#define	BUCKET_SIZE	1024

typedef struct Block
    {
	struct Block *NextBlock;
	int BlockSize;
    }	Block, *BlockP;

#define MIN_SIZE	sizeof(Block)/sizeof(PTR)
#define	NullB		((BlockP)0)


/*  Heap and HeapHeader are public just so that save() and restore()  in
    main.c  can  save  and reload the heap.  main.c actually thinks that
    Heap is a character array.  That mild delusion is essential to  Perq
    running.  Savability is why Heap is a record, too.  heap0 used to be
    duplicated in a 'bottom' field in the Heap record.  For a space file
    which is independent of C-Prolog it should be put back.
*/
struct
    {
	BlockP free[CHAINS+1];
	PTR    top;
	ProLong   used;
    }	Heap;

ProLong HeapHeader = sizeof Heap;

#define FreeChain	Heap.free
#define Top		Heap.top
#define Bottom		heap0
#define FreeMisc	Heap.free[CHAINS]

void InitHeap()
    /* Initialize space tables.  Do it only once. */
    {
	register int i = CHAINS+1;
	while (--i >= 0) FreeChain[i] = NullB;
	Top = heap0;
	Heap.used = 0;
    }


#if	ERRCHECK

#define NOOVERLAP	0
#define SAME		1
#define OVERLAP		2

 int overlap(a, b)
    register PTR a, b;
    /* Check for overlap of space in error checking version */
    {
	register BlockP n;
	int i;

	for (i = CHAINS+1; --i >= 0;)
	    for (n = FreeChain[i]; n != NullB; n = n->NextBlock)
		if (b >= (PTR)n && a-n->BlockSize < (PTR)n)
		    return a == (PTR)n ? SAME : OVERLAP;
	return NOOVERLAP;
    }

#define err(ms)	{ ProError("%s: %s\n",function,ms); abort(); }
#endif	ERRCHECK


void release(Base, size)
    PTR Base;
    register int size;
    {
	register BlockP base = (BlockP)Base;
	register BlockP *list;

#if	ERRCHECK
	ProMessage("%lx\t%d\t%ld\trelease\n", base, ++clock, size);
	fflush(stdout);
	if (ReleaseCheck) {
	    char *function = "release";
	    if (size < MIN_SIZE) err("size < MIN_SIZE\n")
	    if (Base < Bottom || Base+size > Top) err("out of bounds")
	    if (Unsigned(Base) & 3) err("misaligned")
	    switch (overlap(Base, Base+size-1)) {
		case OVERLAP:   err("space overlap")
		case SAME:      err("space freed twice");
		case NOOVERLAP: break;
	    } 
	}
#endif	ERRCHECK
	list = size < CHAINS+MIN_SIZE ? &FreeChain[size-MIN_SIZE] : &FreeMisc;
	base->NextBlock = *list,
	base->BlockSize = size,
	*list = base;
	Heap.used -= size;
    }


 void CollectGarbage()
    /* The garbage collection driver */
    /* NB: it is really just a free-list compactor; it does not reclaim */
    /* space on the stacks.  Heap space is reference counted.  */
    {
	typedef struct GCPair
	    {
		BlockP FirstBlock, LastBlock;
	    }	GCPair;
	typedef GCPair *GCPairP;

	GCPairP Garbage = (GCPairP)(vrz == NullP ? auxstk0 : vrz);
	int Buckets = (Top-Bottom)/BUCKET_SIZE;
	int i;
	register BlockP c, q;

	if ((Top-Bottom)%BUCKET_SIZE != 0) Buckets++;
#if	ERRCHECK
	ProMessage("CollectGarbage %lx[%d]%lx\n", Bottom,Buckets,Top);
	fflush(stdout);
#endif	ERRCHECK
	if (Garbage+Buckets > (GCPairP)auxmax) return;
	for (i = 0; i < Buckets; i++)
	    Garbage[i].FirstBlock = Garbage[i].LastBlock = NullB;

	/*  Sort all free lists onto Garbage by address */

	{
	    register BlockP *p;
	    register GCPairP s;

	    for (i = CHAINS; i >= 0; i--) {
		while (c = FreeChain[i]) {
		    FreeChain[i] = c->NextBlock;
		    s = &Garbage[((PTR)(c)-Bottom)/BUCKET_SIZE];
		    p = &(s->FirstBlock);
		    while ((q = *p) != NullB && q < c) p = &(q->NextBlock);
		    if (!q) s->LastBlock = c;
		    c->NextBlock = q;
		    *p = c;
		    Heap.used += c->BlockSize;
		}
	    }
	}

	/* Merge adjacent pieces of space on the sorted list Garbage */

	{
	    register GCPairP Garb = Garbage;

	    for (i = Buckets; i >= 0; i--) {
		if (Garb[i].FirstBlock == NullB) {
		    Garb[i] = Garb[i+1];
		    Garb[i+1].FirstBlock = Garb[i+1].LastBlock = NullB;
		} else
		if (Garb[i+1].FirstBlock != NullB) {
		    Garb[i].LastBlock->NextBlock = Garb[i+1].FirstBlock;
		    Garb[i+1].FirstBlock = Garb[i+1].LastBlock = NullB;
		}
	    }
	    for (q = Garb[0].FirstBlock; q != NullB; ) {
		if ((BlockP)((PTR)(q)+(q->BlockSize)) != q->NextBlock) {
		    q = q->NextBlock;
		} else {
		    c = q->NextBlock;
		    q->BlockSize += c->BlockSize,
		    q->NextBlock = c->NextBlock;
		}
	    }
	}

	/* Release all items on the merged, sorted list Garbage */

	{
#if	ERRCHECK
	    int t = ReleaseCheck;

	    ReleaseCheck = FALSE;	/* may need to remove this for tough bugs */
#endif	ERRCHECK
	    q = Garbage[0].FirstBlock;
	    while (q != NullB) {
		c = q->NextBlock;
		if (c == NullB && (PTR)(q)+(q->BlockSize) == Top) {
		    Top = (PTR)(q);
		    Heap.used -= q->BlockSize;
		}
		release((PTR)q, q->BlockSize);
		q = c;
	    }
#if	ERRCHECK
	    ReleaseCheck = t;
#endif	ERRCHECK
	}
    }


/*  Note: C-Prolog never asks getsp for more than about 200 words.
    That is why the argument is 'int' rather than 'long'.  If you
    use this function on a system where int=short and you want to
    allocate more than 32k *words* at a time, change 'int'->'long'.
    Change too the argument of release, and the BlockSize field.
*/
PTR getsp(size)
    register int size;
    {
	int gc = 2;
	register PTR result;
	register BlockP b, *l;

	if (size < MIN_SIZE) {
	    ProError("! Internal error: heap request too small");
	    Stop(TRUE);
	}
	while (gc > 0) {
	    if (size < CHAINS+MIN_SIZE) {
		/*  First try getting an exact fit from the right list  */
		b = FreeChain[size-MIN_SIZE];
		if (b != NullB) {
		    FreeChain[size-MIN_SIZE] = b->NextBlock,
		    result = (PTR)b;
		    goto found;
		}
		/*  Next try getting space from the top of the heap  */
		result = Top;
		if (result+size <= hpmax) {
		    Top += size;
		    goto found;
		}
		/*  Take the first fit from the Misc list  */
		for (l = &FreeMisc; b = *l; l = &(b->NextBlock)) {
		    if (b->BlockSize == size) {
			*l = b->NextBlock,
			result = (PTR)b;
			goto found;
		    } else
		    if (b->BlockSize >= size+MIN_SIZE) {
			*l = b->NextBlock,
			Heap.used += b->BlockSize-size;
			release((PTR)b+size, b->BlockSize-size);
			result = (PTR)b;
			goto found;
		    }
		}
	    } else {
		/*  Take the first fit from the Misc list  */
		for (l = &FreeMisc; b = *l; l = &(b->NextBlock)) {
		    if (b->BlockSize == size) {
			*l = b->NextBlock,
			result = (PTR)b;
			goto found;
		    } else
		    if (b->BlockSize >= size+MIN_SIZE) {
			*l = b->NextBlock,
			Heap.used += b->BlockSize-size;
			release((PTR)b+size, b->BlockSize-size);
			result = (PTR)b;
			goto found;
		    }
		}
		/*  Next try getting space from the top of the heap  */
		result = Top;
		if (result+size <= hpmax) {
		    Top += size;
		    goto found;
		}
	    }
	    CollectGarbage();
	    gc--;
	}
	NoSpace(HeapId);
found:
#if	ERRCHECK
	ProMessage("%lx\t%d\t%ld\tgetsp\n", result, ++clock, size);
	fflush(stdout);
	if (Unsigned(result) & 3) {
	    ProError("getsp misalignment\n"); abort();
	}
#endif	ERRCHECK
	/* ClearMem(result, size) used to go here, but whenever C-Prolog */
	/* calls getsp() it immediately fills in every field, so no need */
	Heap.used += size;
	return result;
    }


void RelocHeap(delta)
    register ProLong delta;
    /*  Relocates the free space chains.  */
    /*  The actual free space has already been moved.  */
    /*  heap0 (== Bottom) has been updated already.  */
    /*  delta is an integer offset, not a word offset  */
    {
	register int i;
	register BlockP *p;

	for (i = CHAINS+1; --i >= 0;) {
	    p = &FreeChain[i];
	    while (*p != NullB) {
		*p = (BlockP)((ProLong)*p + delta);
		p = &((*p)->NextBlock);
	    }
	}
	Top = (PTR)((ProLong)Top + delta);
    }


ProLong HeapUsed()
    {
	return Heap.used;
    }


PTR HeapTop()
    {
	return Heap.top;
    }


#if 0
	This package is based on a CMU thesis, and contains nothing that
	really deserves a copyright  notice.   The  rest  of  this  file
	contains code that C-Prolog either never used or used to use but
	uses  no  longer.   The  text  is  left here in case you find it
	useful.  For almost all applications it is superior to malloc().

/*  These simulate the C library heap allocation functions using our own
    heap -- not used at the moment, but should perhaps be used  so  that
    the  I/O  routines  will  not  interfere  with us?  Of course we can
    ensure *that* using setbuf(.).
*/

char *malloc(size)
    unsigned ProLong size;
    {
	unsigned ProLong n = Words(size)+1;
	register PTR p = getsp(n);
	if (p == NullP) return NullS;
	*(ProLong*)p = n;
	return (char *)(p+1);
    }


char *calloc(nelem, elsize)
    unsigned ProLong nelem, elsize;
    {
	return malloc(nelem*elsize);
    }


free(ptr)
    PTR ptr;
    {
	release(ptr, ptr[-1]);
    }


#if	vax & unix
#   define ASM	1	/* Use the assembly code inserts in Vax/Unix */
#else	!vax | !unix
#   define ASM	0	/* but stick to C elsewhere */
#endif	vax & unix

/*  This next routine used to declare char *loc.  I have changed  it  to
    PTR  *loc  so  that it will be about 4 times as fast on non VAX/UNIX
    systems.  There should be no change in speed on VAX/UNIX systems, as
    the multiplication by 4 has to be done somewhere.
*/

ClearMem(loc, num)
    PTR *loc;
	ProLong num;
    {
#if	ASM
	asm(" ashl $2,8(ap),r0 ");
	asm(" movc5 $0,*4(ap),$0,r0,*4(ap) ");
#else	ASM
	register PTR *l = loc;
	register ProLong n = num;

	while (--n >= 0) *l++ = NullP;
#endif	ASM
    }


CopyMem(from, to, num)
    char *from, *to;
	ProLong num;
    {
#if	ASM
	asm(" movc3 12(ap),*4(ap),*8(ap) ");
#else	ASM
	register char *f = from, *t = to;
	register ProLong n = num;
    
	if (f < t) {
	    f += n;
	    t += n;
	    while (--n >= 0) *--t = *--f;
	} else {
	    while (--n >= 0) *t++ = *f++;
	}
#endif	ASM
    }


char *realloc(ptr, size)
    register PTR ptr;
    unsigned ProLong size;
    {
	register ProULong new = Words(size), old = ptr[-1]-1;
        
	if (new < old) {
	    release(ptr+new, old-new);
	    ptr[-1] -= old-new;
	} else
	if (new > old) {
	    register PTR p = getsp(new+1);
	    p[0] = new+1;
	    CopyMem(ptr, p+1, sizeof(PTR)*old);
	    release(ptr-1, old+1);
	    return (char *)(p+1);
	}
	return (char *)ptr;
    }

#endif	0

