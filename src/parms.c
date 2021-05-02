/************************************************************************
*									*
*		   C Prolog     parms.c					*
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
*  Copyright (C) 1984 R.A.O'Keefe.					*
*									*
************************************************************************/

#include "pl.h"

/* Declaration and properties of work areas */

#define K	1024
#define MULTIPL	64


PTR Origin[NAreas+1];	/*  Where the areas start in memory  */
PTR Limit[NAreas];	/*  Each area ends a little before the next begins  */

//#define	AuxSz	 10*K	/*  Size of the auxiliary stack (ca. 4k preds)  */
//#define TrSz	 40*K	/*  Size of the trail  */
//#define	AtomSz	 40*K	/*  Size of the atom area (ca. 6k atoms)  */
//#define HeapSz	 64*K	/*  Size of the heap  */
//#define	GlbSz	128*K	/*  Size of the global stack  */
//#define	LclSz	 64*K	/*  Size of the local stack  */

#define	AuxSz	 16*MULTIPL*K	/*  Size of the auxiliary stack (ca. 4k preds)  */
#define TrSz	 48*MULTIPL*K	/*  Size of the trail  */
#define	AtomSz	 64*MULTIPL*K	/*  Size of the atom area (ca. 6k atoms)  */
#define HeapSz	256*MULTIPL*K	/*  Size of the heap  */
#define	GlbSz	256*MULTIPL*K	/*  Size of the global stack  */
#define	LclSz	128*MULTIPL*K	/*  Size of the local stack  */


#if	BACKWARDS

char *AreaName[NAreas] =
    {
	"aux. stack",	"trail",	"global stack",
	"local stack",	"heap",		"atom space"
    };
ProLong Size[NAreas] =
    {
	AuxSz,		TrSz,		GlbSz,
	LclSz,		HeapSz,		AtomSz
    };
 char Options[] =
	"bdqcxtglha";

#else  ~BACKWARDS

char *AreaName[NAreas] =
    {
	"aux. stack",	"trail",	"atom space",
	"heap",		"global stack",	"local stack"
    };
ProLong Size[NAreas] =
    {
	AuxSz,		TrSz,		AtomSz,
	HeapSz,		GlbSz,		LclSz
    };
char Options[] = "bdqcxtahgl";

#endif	BACKWARDS

	/*	b = boot, q = quiet start-up, d = debug, c = trace Consult,
		A = atom size, X = aux, T = trail, H = heap, G = global,
		L = local.  Area sizes are given in Kbytes	*/

#define	Switches	4
int	State[Switches] = {0, 0, 0, 0};

/* Magic sequence that starts saved states */
/* On 4.1 and 4.2 BSD systems, this lets us execute saved states */

char	savemagic[] = "#!/u4/prolog/prolog\n";
int	saveversion = 22;
char version_0[] = "C Prolog version 1.4e.edai";
char version_1[] = "(C) University of Edinburgh";
char version_2[] = "1984 written by F.Pereira, L.Damas, L.Byrd and R.A.O'Keefe.";
char version_3[] = "2020 adapted to x64 by cprolog.com";

char		/*  default file names */
		BootFile[] = "pl/init",
		InitFile[] = "startup",
		UserFile[] = "%s/.prolog",
		SavedFmt[] = "/usr/lib/prolog/saved_states.d/%s";
#define		UserHome     getenv("HOME")


 int 
numval (char **q)
    {
	register char *p = *q;
	register int n = 0;

	if (*++p == '=') p++;
	while (*p >= '0' && *p <= '9') n *= 10, n -= '0', n += *p++;
	if ((*p|32) != 'k') p--;
	*q = p;
	return n;
    }


 int 
findch (register int c, register char *s)
    {
	register int n = 0;
	c |= 32;		/* force lower case */
	while (*s) if (*s++ == c) return n; else n++;
	return -1;
    }


char *
crack (int argc, char **argv)
    {
	extern char *index();			/* see string(3) in Unix manual */
	char *which_file = NullS;
	char *p;
	int  i, s;

	ProError("%s %s %s\n", version_0, ProPlatform, version_1);
	ProError("%s\n", version_2);
	ProError("%s\n", version_3);
	while (--argc > 0) {			/* for each argument */
	    if (**++argv != '-') {		/* it is a file name */
		which_file = *argv;
		continue;
	    }
	    for (p = *argv; *++p; ) {		/* for each flag */
		i = findch(*p, Options);	/* which is it? */
		if (i < 0) {			/* unknown */
		    if (findch(*p, "-,: ") >= 0) continue;
		    ProError("! Unknown switch: %c in %s\n", *p, *argv);
		    break;
		} else
		if (i < Switches) {		/* -flag */
		    State[i] = TRUE;
		} else {			/* -area=sizeK */
		    s = numval(&p)*K;
		    if (s == 0) {
			ProError("! Missing number at %s\n", *argv);
			break;
		    }
		    Size[i-Switches] = s;
		}
	    }
	}
	/*  The command line has been scanned.  We now have to make sense  */
	/*  of the file name.  The main reason for moving this code here   */
	/*  from main.c was so that these parameters could remain hidden.  */

	if (InBoot) {
	    if (which_file == NullS) which_file = BootFile;
	    if (!Exists(which_file)) {
		ProError("! Bootstrap file %s does not exist.\n", which_file);
		exit(1);
	    }
	} else {
	    State[QUIET] = TRUE;
	    if (which_file == NullS) {
	    	State[QUIET] = FALSE;
	    	sprint(OutBuf, UserFile, UserHome);
	    	if (Exists(OutBuf)) {
	    		which_file = OutBuf;
	    	} else {
	    		which_file = InitFile;
	    	}
	    }
	    if (!Exists(which_file)) {
	    	sprint(OutBuf, SavedFmt, which_file);
	    	if (*which_file == '/' || !Exists(OutBuf)) {
	    		ProError("! Saved state %s does not exist.\n", which_file);
	    		exit(1);
	    	}
	    	which_file = OutBuf;
	    }
	}
	/*  Now we know that which_file is defined and exists  */

	return which_file;
    }

