/************************************************************************
*									*
*                  C Prolog    sysbits.c				*
*                  ========    ---------				*
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
|    This  is  the  UNIX/PaNiX/Xenix/VMS  interface   module.    Stdio	|
| routines  are called all over the place, and there is an unavoidable	|
| call to setjmp(3) in main(), but apart from that  C  Prolog  doesn't	|
| depend on UNIX.							|
|									|
|    Changes made by R.A.O'Keefe on Monday March 14th, 1983, 8:47:08pm	|
|									|
|    I have become so IRRITATED by the fact that neither  Prolog,  nor	|
| UNIX, nor the terminals we use, folds lines, that I have been forced	|
| to  do  something  about  it.   As  the most changeable of the three	|
| culprits is C-Prolog, that is what I have changed.			|
|									|
|    As an arbitrary figure, I have chosen 78 characters as the  width	|
| of  a  terminal.  Nothing should depend on this.  In case people had	|
| programs that relied on knowing that Prolog  didn't  generate  extra	|
| lines,  I only fold lines written to STDOUT; thus telling '/dev/tty'	|
| will work as it always did.  This may change.				|
|									|
|    Furthermore, it seems like a good idea to make  use  of  chdir(),	|
| given  that  it  exists,  and  that I often find myself in the wrong	|
| directory.								|
|									|
|    More changes by R.A.O'Keefe on Thursday May 19th,  1983,  7:53:33	|
| pm.  I have reindented this file so that I can see what is going on,	|
| and  have  re-ordered the procedures for the same reason.  As far as	|
| is possible, procedures are defined before their first use.		|
|									|
|    Another change: a file can be opened  for  writing  at  the  end,	|
| which is new in Prolog, but old in <stdio>, and very useful.  I have	|
| turned  a  couple  of  procedures into macros.  It doesn't make much	|
| difference, and it seemed like fun at the time.			|
|									|
|    Filename expansion in the style of the C-shell is now supported &  |
| I have followed Poplog in allowing $-expansion as well. So a file or  |
| directory name can start with one of four prefixes:			|
|	~	~foo	$	$foo					|
| where $ is the HOME directory and ~ the login directory.		|
| Something of the sort can doubtless be done on VMS, how I know not.	|
|									|
|    Once upon a time Prolog didn't bother keeping track of what line   |
| it had reached in a file.  I have now changed this.  Instead of the   |
| FileMode, we keep the FileLine, with the convention that		|
|	input:   FileLine[i] = current line # >= 1			|
|	output:  FileLine[i] = -1					|
|	closed:  FileLine[i] = 0					|
| So we no longer use bitmasks to test for a file state, and we get the |
| best of both worlds.  The procedure SyntErrPos prints the file name   |
| and line number, it is here so that things don't need exporting.	|
| The current line number is now available as current_line_number(F,N)	|
| and fails if F is not an atom naming a file open for input.		|
|									|
+----------------------------------------------------------------------*/

#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <stdarg.h>
#include "pl.h"

#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>

#ifdef FOLD
#define	TtyWidth	78
	int	TtyLeft;
#endif	FOLD
	int	IsaTty;

enum { READ = 1, WRITE, APPEND };


#define PromptSize	81

#define MaxFile	15

char PlPrompt[PromptSize];

jmp_buf	ZeroState;

extern int PrologEvent;

/* stream information */

	FILE	*File[MaxFile];
	ATOMP	FileAtom[MaxFile];
	int	FileLine[MaxFile];	/* line number or mode */
	byte	FileLock[MaxFile];	/* depth of protection */

	int	NewLine;	/* refers to terminal state */

	char	OutBuf[256];	/* general purpose buffer */
	int	Input;		/* current input stream */
	int	Output;		/* current output streams */
	int	crit = 0;	/* are we in a critical region? */

 void Interrupt();


/*----------------------------------------------------------------------+
|									|
|		Error and Event/Signal handling				|
|									|
+----------------------------------------------------------------------*/

/*  Return the error message for system error #errno  */

const char *
SysError (void)
    {
	return "Unknown Unix error";
    }


/*  stop, and perhaps produce a dump (it's HUGE)  */

void 
Stop (int dump)
    {
	if (dump || State[DEBUG]) {
	    ProError("\nInternal error: dumping core.\n");
	    abort();
	}
	Halt(1);
    }


/*  jump back to the "Event Handler" in main()  */

#define Unwind(x)	longjmp(ZeroState,1)


/*  "signal" a Prolog Event, recovering if possible  */
/*  "crit" means that recovery is not possible, beware of critical regions  */

void 
Event (int n)
    {
	if (State[DEBUG]) ProError("\n Prolog event %d\n", n);
	if (!running) {
	    if (errno != 0) perror("Prolog");
	    ProError("\nError while starting up Prolog -- cannot continue\n");
	    Stop(0);
	}
	if (crit) {
	    ProError("\nError in critical region -- cannot continue\n");
	    ProError("%s\n", OutBuf);	/* ???pretty dubious */
	    Stop(0);
	}
	PrologEvent = n;
	Unwind();
    }


/*  Signal an I/O error; with specific/standard message  */

#define IODie(s)	ErrorMess = s, Event(IO_ERROR)
#define IOError(s)	ErrorMess = SysError(), Event(IO_ERROR)


/*  Report that memory partition has run out.  Should this be here?  */
/*  This used to be declared as type PTR.  It is not so used, though */

void 
NoSpace (int s)
    {
	Ignore sprint(OutBuf, "\n! Out of %s during execution.\n",
	    s >= 0 && s < NAreas ?  AreaName[s] : "?space?");
	ErrorMess = OutBuf;
	Event(GEN_ERROR);
	return;
    }


/*  turn UNIX signals into Prolog Events or interrupts  */
void 
TakeSignal (int s)
    {
	if (State[DEBUG]) ProError("\nSignal %d\n", s);
	switch (s) {
	    case SIGINT:
		Interrupt();
		break;
//	    case SIGQUIT:
//		Stop(FALSE);
	    case SIGFPE:
		Ignore signal(SIGFPE, TakeSignal);
		ArithError("Floating point exception");
	    case SIGSEGV:
		NoSpace(-1);
	    default:
		ProError("\nUnexpected signal: %d\n", s);
		Stop(TRUE);
	}
    }


/*  Tell UNIX to call TakeSignal on certain errors  */

void 
CatchSignals (void)
    {
	Ignore signal(SIGINT, TakeSignal);
	Ignore signal(SIGFPE, TakeSignal);
    }


/*  report a syntax error on the current input channel  */

void 
SyntErrPos (void)
    {
	ProError("\n*** Syntax error at line %d of %s ***\n",
	    FileLine[Input],
	    FileAtom[Input] == AtomP(0) ? "bootstrap" : FileAtom[Input]->stofae);
    }



/*----------------------------------------------------------------------+
|									|
|	File name expansion.						|
|	The expansion is either the original string or a  buffer. |
|	If the expansion fails, "invalid file specification" is said.	|
|									|
+----------------------------------------------------------------------*/

	char BadFileSpec[] = "Invalid file specification";

#include <pwd.h>
extern	struct passwd *getpwuid(), *getpwnam();
extern	__uid_t	getuid();

	int	UserLookedUp = 0;	/* have we expanded ~ ? */
	char	UserLoginDir[80];	/* what did ~ expand to? */
	char	FredUserName[20];	/* what ~fred did we last expand? */
	char	FredLoginDir[80];	/* what did ~fred expand to? */

 char *
CopyPrefix (register char *from, register char *to)
    {
	register int c;

	while ((c = *++from) && c != '/') *to++ = c;
	*to = 0;
	return from;
    }

char *
expand_file (register char *Fancy)
    {
	 char Expanded[120];

	if (Fancy[0] == '~') {
	    register struct passwd *pwent;

	    Fancy = CopyPrefix(Fancy, Expanded);
	    if (Expanded[0] == 0) {		/* ~ */
		if (!UserLookedUp) {		/* not done ~ before */
		    UserLookedUp = getuid();
		    pwent = getpwuid(UserLookedUp);
		    if (!pwent) IODie(BadFileSpec);
		    Ignore strcpy(UserLoginDir, pwent->pw_dir);
		}
		Ignore strcpy(Expanded, UserLoginDir);
	    } else {				/* ~fred */
		if (strcmp(Expanded, FredUserName)) {
		    pwent = getpwnam(Expanded);
		    if (!pwent) IODie(BadFileSpec);
		    Ignore strcpy(Expanded, FredUserName);
		    Ignore strcpy(FredLoginDir, pwent->pw_dir);
		}
		Ignore strcpy(Expanded, FredLoginDir);
	    }
	} else
	if (Fancy[0] == '$') {
	    register char *envent;

	    Fancy = CopyPrefix(Fancy, Expanded);
	    envent = getenv(Expanded[0] ? Expanded : "HOME");
	    if (!envent) IODie(BadFileSpec);
	    Ignore strcpy(Expanded, envent);
	} else {
	    return Fancy;
	}
	{
	    register char *t = Expanded;
	    while (*t++) ; t--;	    
	    if (t[-1] == '/') *--t = 0;
	    Ignore strcpy(t, Fancy);
	    return Expanded;
	}
    }

/*----------------------------------------------------------------------+
|									|
|			I/O proper					|
|									|
+----------------------------------------------------------------------*/

#define set_file(i, chan, mode, lock, atom) \
	File[i] = chan, FileLine[i] = mode,\
	FileLock[i] = lock, FileAtom[i] = atom

void InitIO()
{
	register int i;
    
	for (i = MaxFile; --i >= 0; ) set_file(i, NullF, 0, 0, AtomP(0));
	set_file(STDIN,  stdin,   1, 1, useratom);
	set_file(STDOUT, stdout, -1, 1, useratom);
	set_file(STDERR, stderr, -1, 1, AtomP(0));
	Input = STDIN; Output = STDOUT;
	NewLine= FALSE;
	/*  We want to check for one of two things: I/O coming from
	    the terminal, and I/O coming through pipes.  The latter
	    case might arise when using "script".  There is no good
	    way in UNIX of checking whether an fd is associated with
	    a pipe, but the following hack seems to work: A pipe
	    shows up as a regular file with device = 0.0, uid = 0,
	    gid = 0, and nlink = 0, while terminals show up as a
	    character special with device = 0.0 and nlink >= 1.  The
	    st_rdev field seems to be "real device".
	    IsaTty = isatty(fileno(stdout)); was once used but it is
	    not apparently aware of terminals.
	    4.1bsd (and I think S3) has no need to setbuf(stdout, NullS)
	    as it is smart enough to flush stdout whenever it reads
	    from stdin.
	*/
	{	
	    struct stat statbuf;
	    IsaTty = fstat(0, &statbuf) || statbuf.st_dev == 0;
	}
#ifdef FOLD
	TtyLeft = TtyWidth;
#endif	FOLD
	IsaTty = TRUE;
    setbuf(stdout, NullS);
    setbuf(stderr, NullS);
}

int ProMessage(const char *fmt, ...)
{
	int ret_status = 0;
    va_list args;
    va_start(args, fmt);
    ret_status = vfprint(stdout, fmt, args);
    va_end(args);
    fflush(stdout);
    return ret_status;
}

int ProError(const char *fmt, ...)
{
	int ret_status = 0;
    va_list args;
    va_start(args, fmt);
    ret_status = vfprint(stderr, fmt, args);
    va_end(args);
    fflush(stderr);
    return ret_status;
}

/*  There used to be a very nasty bug, where  you  could  close  a  file
    while  a  broken  state was hanging onto it.  This has been cured by
    adding a Lock to each file: while a file is locked (its lock count >
    0) it is not possible to close it.  This  assumes  that  breaks  are
    never nested more than 255 deep.
*/
void 
LockChannels (int lock)
{
	int i;

	switch (lock) {
	    case 0:
	    	FileLock[Input]--, FileLock[Output]--;
	    	return;
	    case 1:
	    	FileLock[Input]++, FileLock[Output]++;
	    	return;
	    case 2:
	    	for (i = MaxFile; --i >= 0; FileLock[i] = 0) {};
		    FileLock[STDOUT] = FileLock[STDIN] = FileLock[STDERR] = 1;
		    return;
	}
}


 void 
CClose (register int i)
{
	if (FileLock[i]) {
	    clearerr(File[i]);
	} else {
	    Ignore fclose(File[i]);
	    set_file(i, NullF, 0, 0, AtomP(0));
	}
	if (i == Input)  Input  = STDIN; else
	if (i == Output) Output = STDOUT;
}


void 
PClose (ATOMP file)
    {
	register int i;

	/*  There is no need to check for 'user' specially,  */
	/*  as those files are locked. and can't be closed.  */
	for (i = MaxFile; --i >= 0; )
	    if (FileAtom[i] == file)
		CClose(i);
    }


void 
CloseFiles (void)
    {
	register int i;

	for (i = MaxFile; --i >= 0; )
	     if (FileLine[i] != 0)
		CClose(i);
    }


void 
Seen (void)
    {
	CClose(Input);		/* sets Input = STDIN */
    }				/* just clearerr-s stdin */


void 
Told (void)
    {
	CClose(Output);		/* sets Output = STDOUT */
    }				/* just clear-errs stdout or stderr */


ATOMP 
Seeing (void)
    {
	return FileAtom[Input];
    }


ATOMP 
Telling (void)
    {
	return FileAtom[Output];
    }


char *
AtomToFile (
    register ATOMP file		/* well, let's hope... */
)
    {
	if (IsPrim(file) || !IsAtomic(file)) IODie(BadFileSpec);
	return expand_file(file->stofae);
    }


 int 
COpen (
    char *title,
    int mode		/* READ|WRITE|APPEND */
)
    {
	register int i;
	char *cmode = mode==READ ? "r" : mode==WRITE ? "w" : "a";

	i = MaxFile;
	while (--i >= 0 && FileLine[i] != 0) ;
	if (i < 0) IODie("Too many files open");
	if ((File[i] = fopen(title, cmode)) == NullF) IOError();
	FileLine[i] = mode == READ ? 1 : -1,
	FileLock[i] = 0;
	return i;
    }


void 
CSee (				/* only used by the bootstrap loader */
    char *title
)
    {
	Input = COpen(title, READ);
    }


void 
Flush (register PTR file)
    {
	register int i = MaxFile;

	if (!IsAtomic(file)) IODie(BadFileSpec);
	if (IsPrim(file)) {
	    while (--i >= 0)
		if (FileLine[i] < 0 && isatty(fileno(File[i])))
		    Ignore fflush(File[i]);
	} else {
	    while (--i >= 0)
		if (FileLine[i] < 0 && FileAtom[i] == AtomP(file))
		    Ignore fflush(File[i]);
	}
    }


void 
See (register ATOMP file)
    {
	register int i;
    
	if (file == useratom) {Input = STDIN; return;}
	i = MaxFile;
	while (--i >= 0 && FileAtom[i] != file) ;
	if (i < 0) {		/* not already open */
	    i = COpen(AtomToFile(file), READ);
	    FileAtom[i] = file;
	} else
	if (FileLine[i] < 0) {	/* open for output */
	    CClose(i);		/* same as Told() if i=Output */
	    i = COpen(AtomToFile(file), READ);
	    FileAtom[i] = file;
	}
	Input = i;
    }


void 
Tell (register ATOMP file, int append)
    {
	register int i;
    
	if (file == useratom) {Output = STDOUT; return;}
	i = MaxFile;
	while (--i >= 0 && FileAtom[i] != file) ;
	if (i < 0) {		/* not already open */
	    i = COpen(AtomToFile(file), append ? APPEND : WRITE);
	    FileAtom[i] = file;
	} else
	if (FileLine[i] > 0) {	/* open but for input */
	    CClose(i);		/* same as Seen() if i==Input */
	    i = COpen(AtomToFile(file), append ? APPEND : WRITE);
	    FileAtom[i] = file;
	}
	Output = i;
    }


void 
Put (int c)
{
	if (Output == STDOUT) {
	    NewLine = c == '\n';
	    fputc(c, stdout);
	    fflush(stdout);
/*
#ifdef FOLD
	    if (NewLine) {
		TtyLeft = TtyWidth;
	    } else
	    if (--TtyLeft < 0) {
		putchar('\n');
		TtyLeft = TtyWidth-1;
	    }
#endif	FOLD
	    putchar(c);
*/
	} else {
	    register FILE *f = File[Output];
	    putc(c, f);
	}
}


void 
PutString (register char *s)
{
	register int c;

	if (Output == STDOUT) {
	    while(c = *s++) Put(c);
	    fflush(stdout);
	} else {
	    register FILE *f = File[Output];
	    while(c = *s++) putc(c, f);
	    fflush(f);
	}
}


void 
SetPlPrompt (char *s)
    {
	if (s) strncpy(PlPrompt, s, PromptSize-1);
	PlPrompt[PromptSize-1] = '\0';
    }


/*  If C-Prolog is running under "script" we had better make sure
    that prompts do go out even when the channels are pipes.  We
    don't write prompts if the input is coming from a file, and the
    fact that we check the input rather than the output makes the
    bootstrapping session a little tidier.  Prompts never go to
    any channel other than stdout.  This whole area needs a proper
    design rather than the incremental hacking it has received.
*/
void 
Prompt (char *s)
    {
	if (!IsaTty) return;
	PutString(s);
	Ignore fflush(stdout);
    }


void 
PromptIfUser (char *s)
    {
	if (Input == STDIN && Output == STDOUT) Prompt(s);
    }


int 
Get (void)
    {
	register FILE *f = File[Input];
	register int c;			/* char may not hold EOF */

	if (feof(f)) {
	    Seen();
	    Event(END_OF_FILE);
	}
	if (NewLine) PromptIfUser(PlPrompt);
	c = getc(f);
	if (c == EOF) {
	    c = CtrlZ;
	} else
	if (c == '\n') {
	    if (Input == STDIN) {
#ifdef FOLD
		TtyLeft = TtyWidth;
#endif	FOLD
		NewLine = TRUE;
	    }
	    FileLine[Input]++;
	}
	return c;
    }


int 
ToEOL (void)
    {
	register int d, c;
    
	while ((d = getchar()) <= ' ' && d != '\n' && d != EOF) ;
	c = d >= 'A' && d <= 'Z' ? d+('a'-'A') : d;
	while (d != EOF && d != '\n') d = getchar();
	if (d == EOF) clearerr(stdin);
	NewLine = TRUE;
#ifdef FOLD
	TtyLeft = TtyWidth;
#endif	FOLD
	return c;
    }


int 
CurLineNo (register ATOMP file)
    {
	register int i = MaxFile;
	while (--i >= 0 && (FileLine[i] <= 0 || FileAtom[i] != file)) ;
	return i < 0 ? i : FileLine[i];
    }


 void 
Interrupt (void)
    {
#ifdef FOLD
	if (TtyLeft != TtyWidth) {
	    putc('\n', stdout);
	    TtyLeft = TtyWidth, NewLine = TRUE;
	    Ignore fflush(stdout);
	}
#endif	FOLD
	while (TRUE) {
	    Prompt("Action (h for help): ");
	    switch(ToEOL()) {
		case 'a':		/* ABORT */
		    if (crit == 0) Event(ABORT);
		    crit = 2;
		    goto cont;
		case 'b':		/* BREAK */
		    dotrace |= (debug << 1) | 4, debug = TRUE;
		    goto cont;
		case 'c':		/* CONTINUE */
		    goto cont;
		case 's':		/* STATISTICS */
		    Statistics();
		    break;
		case 't':		/* TRACE */
		    sklev = 10000000;
		case 'd':		/* DEBUG */
		    debug = TRUE;
		    goto cont;
		case 'e':		/* EXIT */
		    Halt(0);
		default:
		    ProError("Unknown option, known ones are\n");
		case 'h': case '?':
		    ProError("a\tabort\nb\tbreak\nc\tcontinue\nd\tdebug\n");
		    ProError("e\texit\nh\thelp\nt\ttrace\ns\tstatistics\n");
		    break;
	    }
	}
cont:
	Ignore signal(SIGINT, TakeSignal);
    }



/*----------------------------------------------------------------------+
|									|
|		File-system operations other than I/O			|
|									|
+----------------------------------------------------------------------*/

/*  check whether a file exists or not.  This does not imply that the
    file can be read or written, just that it is there.
*/

int 
Exists (char *title)
    {
	return access(title, 0) == 0;
    }


/*  change the name of a file.  Note that it is not possible to rename
    across devices, though we *could* copy the file and remove the old
    copy.  Prolog treats deletion as a special case of renaming, there
    is no reason to expect the operating system to do so as well.
*/

void 
Rename (char *oldname, char *newname)
    {
	int r;
	if ((r = link(oldname, newname)) == 0 && (r = unlink(oldname)) != 0)
	    unlink(newname);
	if (r != 0) IOError();
    }


void 
Remove (char *title)
    {
	if (unlink(title) != 0) IOError();
    }


/*  UNIX has the idea of a current directory, where searches start unless
    you say otherwise.  VMS has the $set default ... command, which does
    something similar, so there may be a way of implementing ChDir even
    in that antique.  {VMS: Software for the Sixties!}
*/
int 
ChDir (char *newdir)
    {
	extern int chdir();
	return chdir(newdir) == 0;
    }


/*  The ability to invoke another program and come back is one of the
    most important features of UNIX.  Actually doing it involves half
    a dozen fiddly little steps, though there is a library routine to
    do it if you always want /bin/sh.  The VMS code, which I have not
    had a chance to test, but have copied from C-Prolog V1.4, is much
    neater, to my surprise.  No doubt there is mess underneath.  When
    using the Berkeley Unix, you have a choice of shells, and this is
    guaranteed to take you into the one you normally use.
*/

int 
CallShell (char *command)
    {
	int status = -1;		/* child's termination+exit status */
	int child = fork();		/* child's process id */

	if (child == 0) {		/* this is the child process */
	    register int i;		/* for closing files */
	    register char *shell;	/* the shell to be run */

	/*  all open files other than std{in,out,err} must be closed, but  */
	/*  we just want to close the channels, not call fclose(), as the  */
	/*  parent process will do that itself in due course.		   */

	    for (i = MaxFile; --i > STDERR; )
		if (FileLine[i] != 0)
		    Ignore close(fileno(File[i]));
	
	/*  find the path name of the shell from the environment  */

	    shell = getenv("SHELL");
	    if (shell == NullS) shell = "/bin/sh";

	/*  try to execute the shell {no arguments, same standard files}  */

	    if (command == NullS) execl(shell, shell, NullS);
	    else		  execl(shell, shell, "-c", command, NullS);

	/*  if we get here, the execl() call failed  */

	    exits("Failed to open shell");
	}
	{
	    int (*oldsig)() = signal(SIGINT, SIG_IGN);
	    int result =  child < 0 || wait(&status) != child || status != 0;
	    signal(SIGINT, oldsig);
	    return result;
	}
    }    


/*----------------------------------------------------------------------+
|									|
|			Create the memory partitions			|
|	It is useful to align them on page boundaries.			|
|									|
+----------------------------------------------------------------------*/

#define	DatAlign	1024


void 
CreateStacks (void)
    {
	register char *r;	/* char* is the UNIX convention		*/
	register ProLong i, s;

	for (i = NAreas, s = 0; --i >= 0; s += Size[i]) ;
    if ((r = malloc(s)) == nil) {
	    perror("Prolog");
	    exits("Malloc failed");
    }
	for (i = 0; i < NAreas; i++) Origin[i] = (PTR)r, r += Size[i];
	Origin[NAreas] = (PTR)r;
#ifdef BACKWARDS
	auxmax	= tr0	-10;
	trmax	= glb0	-10;
	v1max	= lcl0	-500;
	vmax	= heap0	-500;
	hpmax	= atom0	-100;
	atmax	= (PTR)r-500;
#else  !BACKWARDS
	auxmax	= tr0	-10;
	trmax	= atom0	-10;
	atmax	= heap0	-100;
	hpmax	= glb0	-100;
	v1max	= lcl0	-500;
	vmax	= (PTR)r-500;
#endif	BACKWARDS
    }
    

/*  Despite the UNIX documentation, the magic number you divide times by
    to get seconds is NOT the line frequency.  It is 60, even in the UK,
    where the more rational 50 Hz is used.  It is EXTREMELY unlikely for
    this number to change.
*/
#define Hz	60.0

double 
CpuTime (void)
{
  return 0;
}

