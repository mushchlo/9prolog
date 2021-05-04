/************************************************************************
*									*
*		   C Prolog	rewrite.c				*
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
*  Copyright (C) 1984 F.Pereira, L.Damas, L.Byrd and R.A.O'Keefe.	*
*									*
************************************************************************/

#include "pl.h"
#include <stdio.h>

enum { PREFIX, INFIX, POSTFIX };

/* decrease left priority flag */

#define dlprflg	010000

/* decrease right priority flag */

#define drprflg	004000

/* priority field */

#define mskprty 003777		/* = 2047 > 1200 */

/* Character types for tokeniser */

#define UC	'\01'		/* Upper case alphabetic */
#define UL	'\02'		/* Underline */
#define LC	'\03'		/* Lower case alphabetic */
#define N	'\04'		/* Digit */
#define QT	'\05'		/* Single quote */
#define DC	'\06'		/* Double quote */
#define SY	'\07'		/* Symbol character */
#define SL	'\10'		/* Solo character */
#define BK	'\11'		/* Brackets & friends */
#define BS	'\12'		/* Blank space */

 char chtyp[] = {
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/*  sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    BS, SL, DC, SY, LC, SL, SY, QT, BK, BK, SY, SY, BK, SY, SY, SY,

/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
    N,  N,  N,  N,  N,  N,  N,  N,  N,  N, SY, SL, SY, SY, SY, SY,

/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
   SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, BK, SY, BK, SY, UL,

/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
   SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  del */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BK, BK, BK, SY,  BS };


/*  Scan the string *s to see if it is a number.  If so, *p takes
    the constructed number, and TRUE is returned, otherwise FALSE.
    If free is FALSE, the number must reach the end of the string
    to be accepted.  If the string is accepted as a number, *s is
    set to the first remaining character, no change if rejected.
    This function is used here and in name/2
*/
int 
NumberString (char **s, PTR *p, int free)
    {
	register char *t = *s;
	char c; double d; ProLong i;

	if (*t == '+' || *t == '-') t++;
	if (chtyp[*t] != N) return FALSE;
	while (chtyp[*++t] == N) ;
	if (*t == '.' && chtyp[t[1]] == N) {
	    while (chtyp[*++t] == N) ;
	}
	if (*t == 'e' || *t == 'E') {
	    t++;
	    if (*t == '+' || *t == '-') t++;
	    if (chtyp[*t] != N) return FALSE;
	    while (chtyp[*++t] == N) ;
	}
	if (*t && !free) return FALSE;
	c = *t, *t = '\0';
	Ignore sscanf(*s, "%lf", &d);
	*t = c, *s = t;
	*p = Narrow(d, &i) ? ConsInt(i) : ConsFloat(d);
	return TRUE;
    }


/*  It seems like a good idea to experiment with making the character
    table available; of course we won't TELL anyone about it... What I
    really want it for is to zap the $ sign; in the initial load it 
    should be a lower-case letter, but afterwards it should be a symbol,
    and wasn't.  Testing InBoot is insufficient.
*/
int 
GetChType (register int ch)
    {
	if (ch >= 0 && ch <= 127) return chtyp[ch];
	ErrorMess = "! chtype(C,_,_): C must be 0..127";
	return -1;
    }

int 
SetChType (register int ch, register int type)
    {
	if (type < 1 || type > 10) {
	    ErrorMess = "! chtype(_,_,T): T must be 1..10";
	    return TRUE;
	} else
	if (chtyp[ch] == N && type != N) {
	    ErrorMess = "! chtype(D,_,T): can't change digits";
	    return TRUE;
	} else {
	    chtyp[ch] = type;
	    return FALSE;
	}
    }


/*  isop(atom,optype)->p,lp,rp
    checks whether the atom has an operator property of the given  sort,
    returning  FALSE  if  it  has  not,  and  returning TRUE and binding
    p,lp,rp if it has.  p is the priority of  the  operator  itself,  lp
    that of its left argument, if any, and rp that of its right argument
    if any.  It has been made public to reimplement current_op.

    maxprio(atom, priority)
    checks whether atom has an operator property whose priority  exceeds
    priority.  This is needed when we write out an atom.  Without it, if
    we  write(+(+,+))  it comes out as + + +, which we cannot read back.
    With it, it comes out as + + (+) which we can read back.
*/

int 
isop (ATOMP atom, int optype, int *p, int *lp, int *rp)
    {
	register int oe, pr;

	switch (optype) {
	    case PREFIX:	oe = atom->prfxofae;	break;
	    case INFIX:		oe = atom->infxofae;	break;
	    case POSTFIX:	oe = atom->psfxofae;	break;
	    default:		return FALSE;
	}
	if (oe == 0) return FALSE;
	*p = pr = oe & mskprty;
	*lp = oe & dlprflg ? pr-1 : pr;
	*rp = oe & drprflg ? pr-1 : pr;
	return TRUE;
    }

 int 
maxprio (register ATOMP atom, register int priority)
    {
	return (atom->prfxofae & mskprty) > priority
	||     (atom->infxofae & mskprty) > priority
	||     (atom->psfxofae & mskprty) > priority;
    }


 struct {char *OpName; short OpType, OpMask;} OpInfo[] = {
		 {"xfx",	INFIX,   dlprflg | drprflg },
		 {"xfy",	INFIX,   dlprflg },
		 {"yfx",	INFIX,	 drprflg },
		 {"xf",		POSTFIX, dlprflg },
		 {"yf",		POSTFIX, 0 },
		 {"fx",		PREFIX,  drprflg },
		 {"fy",		PREFIX,  0 },
    };

int 
op (	/*  process an 'op' declaration  */
    PTR prio,
    PTR optype,
    register PTR spec
)
    {
	register int pr;	/* priority|mask */
	register PTR at;	/* atom to declare */
	int type;		/* prefix/infix/postfix */
	PTR frame;		/* frame of spec */
    
	if (!IsInt(prio)) return FALSE;
	pr = XtrInt(prio);
	if (pr > 1200 || pr < 0) return FALSE;
	if (!IsAtomic(optype) || IsPrim(optype)) return FALSE;
    
    /*  look the operator type up in the operator class table  */
	{
	    register int i;
	    char *ops = AtomP(optype)->stofae;
	    for (i = 6; i >= 0 && strcmp(ops, OpInfo[i].OpName); i--) ;
	    if (i < 0) return FALSE;
	    type = OpInfo[i].OpType;
	    if (pr != 0) pr |= OpInfo[i].OpMask;
	}
	spec = vvalue(spec, &frame);
	while (spec != atomnil) {
	    if (IsPrim(spec) || IsRef(spec)) return FALSE;
	    if (IsAtomic(spec)) {
		at = spec, spec = atomnil;
	    } else {
		if (SkelFuncP(spec) != listfunc) return FALSE;
		at = arg(SkelP(spec)->Arg1, frame);
		if (IsPrim(at) || !IsAtomic(at)) return FALSE;
		spec = argv(SkelP(spec)->Arg2, frame, &frame);
	    }
	    switch (type) {
		case PREFIX:  AtomP(at)->prfxofae = pr; break;
		case INFIX:   AtomP(at)->infxofae = pr; break;
		case POSTFIX: AtomP(at)->psfxofae = pr; break;
	    }
	}
	return TRUE;
    }


 void 
PutAtom (			/* print an atom, quoted if necessary  */
    ATOMP at
)
    {
	if (quoteia) {
	    register char *s = at->stofae;
	    switch (chtyp[*s]) {
		case LC:		/* identifier? */
		    while (*s) if (chtyp[*s++] > N) goto QUOTED;
		    break;
		case SL:		/* solo character? */
		    if (s[0] == '%' || s[1] != '\0') goto QUOTED;
		    break;
		case SY:		/* symbol(s)? */
		    if (!strcmp(s, "/*") || !strcmp(s, ".")) goto QUOTED;
		    while (*s) if (chtyp[*s++] != SY) goto QUOTED;
		    break;
		case BK:		/* '[]' and '{}' are ok  */
		    if (strcmp(s, "[]") && strcmp(s, "{}")) goto QUOTED;
		    break;
		default:		/* others, e.g. quotes, commas...*/
		    goto QUOTED;
	    }
	}
	PutString(at->stofae);
	return;
QUOTED:
	{
	    register char *s = at->stofae;
	    register int c;

	    Put(' ');		/* Cure the 2'2b' and 0','2 problem */
	    Put('\'');
	    while (c = *s++) {
		if (c == '\'') Put(c);
		Put(c);
	    }
	    Put('\'');
	}
    }


/*  spaceout(a)
    writes a space if the atom a (being used as an operator)
    is alphabetic.  IT CAN BE FOOLED, but extra spaces are ok.
    The entire business of spacing needs to be revised.
*/
void 
spaceout (ATOMP a)
    {
	if (chtyp[a->stofae[0]] == LC) Put(' ');
    }


/*  num2chars(k)
    prints k in a suitable numeric format in OutBuf
*/
char *
num2chars (PTR k)
    {
	if IsInt(k)
	    Ignore sprint(OutBuf, "%ld", XtrInt(k));
	else if IsFloat(k)
	    Ignore sprint(OutBuf, "%.5g", XtrFloat(k));	/* WAS %f */
	else
	    Ignore sprint(OutBuf, "#<%lx>", Unsigned(k));
	return OutBuf;
    }


/*  ProPWrite(term, frame, priority)
    writes the term represented by (term,frame).
    NB: it can happen that the term contains LOCAL variables.
    If so, the variable X is assumed to point to the local frame.
*/
void 
ProPWrite (
    register PTR term,		/*  term to be printed  */
    PTR frame,			/*  its global frame  */
    register int priority	/*  of its context  */
)
    {
	FUNCTORP fn;		/*  principal functor  */
	ATOMP at;		/*  its atom  */
	register int arity;	/*  its arity  */
	int p, lp, rp;		/*  priority of op, left/right arg  */
	register PTR ax;	/*  argument  */
	PTR af;			/*  frame of argument  */

	if (IsPrim(term)) {
	    PutString(num2chars(term));
	    return;
	}
	if (IsAtomic(term)) {
	    if (quoteia >= 0 && maxprio(AtomP(term), priority)) {
		Put('('); PutAtom(AtomP(term)); Put(')');
	    } else {
		PutAtom(AtomP(term));
	    }
	    return;
	}
	if (IsUnbound(term)) {
	    if (term >= lcl0) Ignore sprint(OutBuf, "L%ld", Unsigned(term-lcl0));
	    else Ignore sprint(OutBuf, "_%ld", Unsigned(term-glb0));
	    PutString(OutBuf);
	    return;
	}
	if (IsRef(term)) frame = MolP(term)->Env, term = MolP(term)->Sk;
	fn = SkelFuncP(term);
	at = AtomP(fn->atoffe), arity = fn->arityoffe;
	if (quoteia >= 0 && arity <= 2) {
	    if (fn == listfunc) {
		Put('[');
		for (;;) {
		    ax = argv(SkelP(term)->Arg1, frame, &af);
		    ProPWrite(ax, af, 999);
		    term = argv(SkelP(term)->Arg2, frame, &frame);
		    if (IsAtomic(term) || SkelFuncP(term) != listfunc) break;
		    Put(',');
		}
		if (term != atomnil) {
		    Put('|');
		    ProPWrite(term, frame, 999);
		}
		Put(']');
		return;
	    }
	    if (arity == 1) {
		if (at == bracesatom) {
		    Put('{');
		    ax = argv(SkelP(term)->Arg1, frame, &af);
		    ProPWrite(ax, af, 1200);
		    Put('}');
		    return;
		}
		if (isop(at, PREFIX, &p, &lp, &rp)) {
		    if (p > priority) Put('(');
		    PutAtom(at);
		    spaceout(at);
		    ax = argv(SkelP(term)->Arg1, frame, &af);
		    ProPWrite(ax, af, rp);
		    if (p > priority) Put(')');
		    return;
		}
		if (isop(at, POSTFIX, &p, &lp, &rp)) {
		    if (p > priority) Put('(');
		    ax = argv(SkelP(term)->Arg1, frame, &af);
		    ProPWrite(ax, af, lp);
		    spaceout(at);
		    PutAtom(at);
		    if (p > priority) Put(')');
		    return;
		}  
	    }
	    if (arity == 2 && isop(at, INFIX, &p, &lp, &rp)) {
		if (p > priority) Put('(');
		ax = argv(SkelP(term)->Arg1, frame, &af);
		ProPWrite(ax, af, lp);
		spaceout(at);
		PutAtom(at);
		spaceout(at);
		ax = argv(SkelP(term)->Arg2, frame, &af);
		ProPWrite(ax, af, rp);
		if (p > priority) Put(')');
		return;
	    }
	}
	PutAtom(at);
	Put('(');
	while (--arity >= 0) {
	    ax = argv(NextArg(term), frame, &af);
	    ProPWrite(ax, af, 999);
	    if (arity > 0) Put(',');
	}
	Put(')');
    }



/*---------------------------------------------------------------------------

    ProPRead() reads a Prolog term.  It  has  several  auxiliary  functions
    which are listed first.  The way that version 1.2D handles errors is
    different from all earlier versions.  Instead of setting a flag, and
    letting the parser thrash around generating lots more error messages
    (but  using  the  flag  to suppress them), it uses the standard Unix
    long-jump stuff (see setjmp(3)) to return at once.

    Versions of C Prolog prior to 1.2D.edai used to handle the  variable
    dictionary in a very nasty manner.  Nowadays the variable dictionary
    is  just  a linear block of <name,variable> pairs which just happens
    to be held in the auxiliary stack for the short time that it is held
    anywhere.  Instead of using a special hack to get at variable names,
    the top level now calls read/2, which returns  a  list  of  Name=Var
    pairs.   This  is  what  Dec-10  and Prolog-X do as well.  Since the
    variable dictionary ONLY exists while pread() is being called, there
    is no problem of saving and restoring states containing such beasts.
    varn is the number of variable descriptions, varp the latest one.

---------------------------------------------------------------------------*/

/* Token types */
enum { FULLSTOP, NAME, PRIMITIVE, VAR, STRING, PUNCTUATION };

	jmp_buf	ReadHandler;	/* for errors */
	int	tokentype;	/* class FULLSTOP &c of latest token */
	int	retoken;	/* true => re-scan this token */
	PTR	*lsp;		/* Local Stack Pointer */
	char	*line, *lp, *lpmax;
	char	nam[255];	/* text of identifier, string, &c */
	union
    {
	ATOMP	AsATOM;		/* when tokentype = NAME */
	PTR	AsPTR;		/* when tokentype = PRIMITIVE or VAR */
	char	AsChar;		/* when tokentype = PUNCTUATION */
    }	tokeninfo;		/* use `nam` when = STRING */

typedef struct var_dict_entry
    {
	ATOMP	name;
	PTR	var;
    }  *DICTP;

	DICTP	varp;
	int	varn;


/*  Look up a variable name in the variable table.  */

 PTR 
lookupvar (char *id)
    {
	register PTR var;

	if (!strcmp(id, "_")) {
	    var = Regs_v1; VarVal(var) = NullP; GrowGlobal(1);
	} else {
	    register ATOMP name = lookup(id);
	    register DICTP p = varp;
	    register int n = varn;

	    while (--p, --n >= 0) if (p->name == name) return p->var;
	    p = varp++, varn++;
	    if ((PTR)p >= auxmax) NoSpace(AuxId);
	    var = Regs_v1; VarVal(var) = NullP; GrowGlobal(1);
	    p->name = name, p->var = var;
	}
	return var;
    }


/*  report a syntax error and wind things up */

 void 
SyntaxError (void)
    {
	register char *p;
	SyntErrPos();
	for (p = line; p <= lpmax; p++) {
	    if (p == lp) ProError("\n**here**\n");
	    putc(*p, stderr);
	}
	longjmp(ReadHandler, TRUE);
    }


/*  token - tokeniser */

 int 
token (void)
    {
	register int k, ch;
	register char *l = nam;

	if (retoken) {
	    retoken = FALSE;
	    return tokentype;
	}
start:
	switch (chtyp[*lp]) {
	    case BS:
		lp++;
		goto start;
	
	    case UC:		/* uppercase letter */
		k = lc; goto id;
	    case UL:		/* underline */
		k = 1; goto id;
	    case LC:		/* lowercase letter */
		k = 0;
id:		 		/* common to variables and atoms */
		while (chtyp[ch = *lp] <= N) {
		    if (!lc && !k && ch >= 'A' && ch <= 'Z') ch += 32;
		    *l++ = ch, lp++;
		}
		*l = '\0';
		if (k) {
		    tokentype = VAR,  tokeninfo.AsPTR = lookupvar(nam);
		} else {
		    tokentype = NAME, tokeninfo.AsATOM = lookup(nam);
		}
		break;
	
	    case N:			/* digit */
		k = chtyp[lp[1]];
		if ((k == QT || k == DC) && chtyp[lp[2]] == N) {
		    int n = 0;
		    k = *lp-'0', lp += 2;
		    while (chtyp[*lp] == N) n = n*k-'0'+*lp++;
		    tokeninfo.AsPTR = ConsInt(n);
		} else
		if (!NumberString(&lp, &tokeninfo.AsPTR, TRUE)) 
		    SyntaxError();
		tokentype = PRIMITIVE;
		break;
	
	    case QT:		/* single quote */
	    case DC:		/* double quote */
		ch = *lp++;
		while (*lp != ch || *++lp == ch) {
		    *l++ = *lp++;
		    if (l-nam >= 240) SyntaxError();
		}
		*l = '\0';
		if (chtyp[ch] == QT) {
		    tokentype = NAME,   tokeninfo.AsATOM = lookup(nam);
		} else {
		    tokentype = STRING;	/* stringtolist looks in `nam` */
		}
		break;
	
	    case SY:		/* symbol char */
		if (*lp == '.' && chtyp[lp[1]] == BS) {
		    tokentype = FULLSTOP;	/* full stop is a special case */
		    break;
		}
		while (chtyp[*lp] == SY) *l++ = *lp++;
		*l = '\0';
		tokentype = NAME, tokeninfo.AsATOM = lookup(nam);
		break;
	
	    case SL:		/* solo char */
		*l++ = *lp++, *l = '\0';
		tokentype = NAME, tokeninfo.AsATOM = lookup(nam);
		break;
	
	    case BK:		/*  punctuation char */
		k = *lp++;
		if (k == ',' && lp[0] == '.' && lp[1] == '.') {
		    k = '|', lp += 2;
		}		/* ,.. -> | is for NOLC mode */
		while (chtyp[ch = *lp++] == BS) ;
		if (k == '[' && ch == ']') {
		    tokentype = NAME;
		    tokeninfo.AsATOM = nilatom;
		} else
		if (k == '{' && ch == '}') {
		    tokentype = NAME;
		    tokeninfo.AsATOM = bracesatom;
		} else {
		    tokentype = PUNCTUATION, tokeninfo.AsChar = k;
		    lp--;
		}
		break;
	}
	return tokentype;
    }		/* end of tokeniser */


 PTR term()
    /* forward */;


/*  readargs - parse arguments of a term */

 PTR 
readargs (ATOMP atom)
    {
	PTR *savelsp = lsp;
	int arity;

	Ignore token();			/* skip the ( */
	do *lsp++ = term(999);
	while (token() == PUNCTUATION && tokeninfo.AsChar == ',');
	if (tokentype != PUNCTUATION || tokeninfo.AsChar != ')') SyntaxError();
	arity = lsp-savelsp, lsp = savelsp;
	return apply(atom, arity, savelsp);
    }


/*  stringtolist - string to list of chars */

 PTR 
stringtolist (void)
    {
	PTR *savelsp = lsp;
	register int/*char*/ ch;
	register char *l;
    
	if (*nam == '\0') return atomnil;	/* quick patch */
	for (l = nam; ch = *l++; *lsp++ = ConsInt(ch)) ;
	*lsp = atomnil, lsp = savelsp;
	return makelist(l-nam, savelsp);
    }


/*  readlist - parse a prolog list */

 PTR 
readlist (void)
    {
	PTR *savelsp = lsp;
	ProLong n;

	do *lsp++ = term(999);
	while (token() == PUNCTUATION && tokeninfo.AsChar == ',');
	if (tokentype == PUNCTUATION && tokeninfo.AsChar == '|') {
	    *lsp = term(999);
	} else {
	    *lsp = atomnil, retoken = TRUE;
	}
	n = (lsp-savelsp)+1, lsp = savelsp;
	return makelist(n, savelsp);
    }


/*  prefix_is_atom - having read an atom which has a prefix-op property,
    inspect the next token to determine whether it must be taken as an
    atom despite this.  One token of lookahead doesn't seem to be enough.
*/
 int 
prefix_is_atom (
    int n,
    int m		/* context, prefix priority */
)
    {
	int o, l, r;

	switch (tokentype) {
	    case FULLSTOP:
		return TRUE;
	    case PUNCTUATION:
		switch (tokeninfo.AsChar) {
		    case ')': case ']': case '}': case ',': case '|':
			return TRUE;
		    case '(': case '[': case '{': default:
			return FALSE;
		}
	    case NAME:
#define	forced	/* o <= n && */ l >= m
		/*  I am not sure whether to put the second conjunct  */
		/*  in or not.  There are examples which don't work   */
		/*  with it in.  Are there examples which need it???  */
		return isop(tokeninfo.AsATOM, INFIX,  &o,&l,&r) && forced
		    || isop(tokeninfo.AsATOM, POSTFIX,&o,&l,&r) && forced;
	    default:
		return FALSE;
	}
    }


/* term - parse token stream to get term */

 PTR 
term (register int n)
    {
	register int m;
	int  mO, mL, mR;
	PTR e[2];
	register ATOMP s;

	m = 0;
	switch (token()) {
	    case NAME:			/* a name */
		s = tokeninfo.AsATOM;
		if (*lp == '(') {
		    e[0] = readargs(s);
		    break;
		}
		if (isop(s, PREFIX, &mO, &mL, &mR)) {
		    Ignore token(), retoken = TRUE;
		    /*  New hack so -<number> always applies */
		    if (s == Minus && tokentype == PRIMITIVE
			&& IsNumber(tokeninfo.AsPTR)) {
			e[0] = IsInt(tokeninfo.AsPTR)
			     ?	ConsInt(-XtrInt(tokeninfo.AsPTR))
			     :  ConsFloat(-XtrFloat(tokeninfo.AsPTR));
			retoken = FALSE;
			break;
		    }
		    if (mO > n) SyntaxError();
		    if (prefix_is_atom(n, mO)) {
			if (m > n) SyntaxError();
			e[0] = (PTR)s;
			break;
		    }
		    e[0] = term(mR);
		    e[0] = apply(s, 1, e);
		    m = mO;
		    break;
		}
		e[0] = (PTR)s;
		break;
	
	    case PRIMITIVE:		/* a primitive type (already encoded) */
		e[0] = tokeninfo.AsPTR;
		break;
	    
	    case VAR:			/* a variable */
		e[0] = tokeninfo.AsPTR;
		break;
	
	    case STRING:		/* a string */
		e[0] = stringtolist();
		break;
	
	    case PUNCTUATION:			/*  punctuation char */
		if (tokeninfo.AsChar == '(') {
		    e[0] = term(1200);
		    if (token() != PUNCTUATION || tokeninfo.AsChar != ')')
			SyntaxError();
		    break;
		}
		if (tokeninfo.AsChar == '[') {
		    e[0] = readlist();
		    if (token() != PUNCTUATION || tokeninfo.AsChar != ']')
			SyntaxError();
		    break;
		}
		if (tokeninfo.AsChar == '{') {
		    e[0] = term(1200);
		    if (token() != PUNCTUATION || tokeninfo.AsChar != '}')
			SyntaxError();
		    e[0] = apply(bracesatom, 1, e);
		    break;
		}
	
	    case FULLSTOP:	/*  other punctuation chars or fullstop */
		SyntaxError();
	}
on:
	if (token() == NAME) {
	    s = tokeninfo.AsATOM;
	    if (isop(s, INFIX, &mO, &mL, &mR) && mO <= n && mL >= m) {
		e[1] = term(mR);
		e[0] = apply(s, 2, e);
		m = mO;
		goto on;
	    }
	    if (isop(s, POSTFIX, &mO, &mL, &mR) && mO <= n && mL >= m) {
		e[0] = apply(s, 1, e);
		m = mO;
		goto on;
	    }
	    retoken = TRUE;
	    return e[0];
	}
    
	if (tokentype == FULLSTOP) {
	    retoken = TRUE;
	    return e[0];
	}
	if (tokentype != PUNCTUATION || tokeninfo.AsChar == '('
	||  tokeninfo.AsChar == '['  || tokeninfo.AsChar == '{')
	    SyntaxError();
	if (tokeninfo.AsChar == ',' && n >= 1000 && m < 1000) {
	    e[1] = term(1000);
	    e[0] = apply(commaatom, 2, e);
	    if ((m = 1000) < n) goto on;
	} else
	if (tokeninfo.AsChar == '|' && n >= 1100 && m < 1100) {
	    e[1] = term(1100);
	    e[0] = apply(semicatom, 2, e);
	    if ((m = 1100) < n) goto on;
	} else {
	    retoken = TRUE;
	}
	return e[0];
    }		/* end of term */


/*  ProPRead(&names)
    implements the  Prolog  read(X)  and  read(X,Vars)  predicates.   If
    names==CellP(0)  it  is the former, and if names!=CellP(0) it is the
    read/2 predicate, and constructs a  list  of  Name=Var  pairs.   For
    example,  read(_0  is  _1-1, ['Pred'=_0, 'Succ'=_1] is true when the
    text we parsed was  "Pred  is  Succ-1".   If  this  predicate  fails
    (returns NullP) names is not assigned to.  Note that pread() changes
    the external variable v1 only, though it does use the local stack as
    workspace to hold the text of the term and its pointers.
*/

PTR 
ProPRead (PTR *names)
    {
	char*lsz;
	PTR *savelsp;
	PTR  savev1 = Regs_v1;
	PTR  answer;

	savelsp = lsp = CellP(Regs_V)+3;
	lp = line = CharP(lsp);
	lsz = lp+3900;
	varn = 0, varp = (DICTP)vrz;
	*line++ = ' ', lpmax = line;
    
	for (;;) {
	    register int ch, chtype;

	    ch = Get();
char_read:
	    if (ch == '%') {
		ch = Get();
		if (ch == '(') {		/* %( -> { for upper case ttys */
		    ch = '{'; 
		} else
		if (ch == ')') {		/* %) -> } for upper case ttys */
		    ch = '}';
		} else {
		    while (ch != '\n') ch = Get();
		}
	    }
	    chtype = chtyp[ch];
	    if (chtype == BS) {
		while (chtyp[ch] == BS) ch = Get();
		if (lpmax[-1] != ' ') *lpmax++ = ' ';
		goto char_read;
	    }
	    *lpmax++ = ch;
	    if (ch == '*' && lpmax[-2] == '/') {
		lpmax -= 2;
		while (ch != '/') {
		    while (Get() != '*') ;
		    while ((ch = Get()) == '*') ;
		}
	    }
	    if (lpmax > lsz) {
		ProError("\n** Text too long **\n");
		Event(ABORT);
	    }
	    if (chtype == QT || chtype == DC) {
		int left = lsz-lpmax-20;
		if (left > 1024) left = 1024;

		*lpmax++ = ch = Get();
		if (lpmax[-3] == '0') {
		    sprint(lpmax-3, "%3d", ch);
		    chtype = N;
		} else
		if (chtyp[ch] == N && chtyp[lpmax[-3]] == N) {
		    chtype = N;
		} else {
		    while (chtyp[ch] != chtype) {
			if (--left <= 0) {
			    ProError("\n** Missing quote **\n");
			    while (Get() != '.');
			    return NullP;
			}
			*lpmax++ = ch = Get();
		    }
		}
	    }
	    if (ch == '.' && chtyp[lpmax[-2]] != SY) {
		ch = Get();
		if (chtyp[ch] != BS) goto char_read;
		break;
	    }
	}
	*lpmax = '\n', lpmax[1] = '\0';
	lsp += Words(lpmax-lp+1);
	retoken = FALSE;
	if (setjmp(ReadHandler)) {	/* there was an error */
	    answer = NullP, Regs_v1 = savev1;
	} else {			/* all went well */
	    answer = term(1200);
	    if (token() != FULLSTOP) SyntaxError();
	    if (names == CellP(0)) {
	    } else
	    if (varn == 0) {
		*names = atomnil;
	    } else {
		register PTR *nsp = savelsp;
    
		while (--varp, --varn >= 0) {
		    nsp[0] = (PTR)(varp->name),
		    nsp[1] = varp->var;
		    nsp[0] = apply(Equal, 2, nsp);
		    nsp++;
		}
		*nsp++ = atomnil;
		*names = makelist(nsp-savelsp, savelsp);
	    }
	}
	lsp = savelsp;
	return answer;
    }


