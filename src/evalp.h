/************************************************************************
*									*
*                  C Prolog     evalp.h					*
*                  ========	-------					*
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
|   These are the internal codes for the evaluable  predicates.   They	|
|   connect  the  definitions in the bootstrap file(s) to the big case	|
|   statement  following  the  label   EvalPred:   in   main.c.    The	|
|   definitions in the bootstrap file are clauses of the form		|
|									|
|       p(A1,...,Ak) :- n.						|
|									|
|  or calls of the form							|
|									|
|       :- $sysp(p(A1,...,Ak),n).					|
|									|
|   where p is the evaluable predicate being  defined,  and  n  is its	|
|   internal  code (Prolog doesn't know about the symbolic definitions	|
|   below, the actual numbers must be used).  The  difference  between	|
|   the  first  and  second forms of definition is that in the first a	|
|   stack frame is build for a call to the predicate,  containing  the	|
|   arguments,  which  can be accessed in the standard way via the 'x'	|
|   pointer.  In other words, the C code for the  evaluable  predicate	|
|   will  execute  as  if  the  definition  clause was its parent.  In	|
|   contrast, evaluable predicates defined in the second way  have  no	|
|   frame  pushed  for  them, it is up to them to find their arguments	|
|   and execution environment.  These names have all been changed  for	|
|   the benefit of V7 C compilers which only check the first 8 letters	|
|   of  a name.  The have also been completely renumbered, so that the	|
|   numeric order corresponds to the logical and textual order in  the	|
|   evalp.c include file.						|
|   Since then, the numbers have got out of hand again.  Sigh.		|
|									|
+----------------------------------------------------------------------*/

/* 1 - 133 */

enum { _and_ = 1, _hidden_call_, _user_call_,
		_cut_, _repeat_, _abort_, _call1_, _yes_,
		_no_, _halt_, _break_, _break_start_, _break_end_,
		_exit_break_, _user_exec_, _repply_, _fail_, _kcompare_,
		_ncompare_,
		_succ_ = 26, _is_ = 27,
		_var_ = 30, _nonvar_, _integer_, _number_, _primitive_,
		_db_reference_, _atomic_, _atom_, _compound_, _compare_,
		_tcompare_,
		_name_ = 46, _functor_, _arg_, _univ_,
		_z1_assert_, _a1_assert_, _z2_assert_, _a2_assert_, _recordz_,
		_recorda_, _assertr_, _instance_, _erase_, _erased_,
		_clause_, _recorded_ = 62, _catom_ = 64,
		_cfunctor = 66, _abolish_ = 68, _is_op_,
		_see_, _seeing_, _2seeing_, _seen_, _tell_,
		_append_, _telling_, _2telling_, _told_, _close_,
		_read_, _get0_, _get_, _skip_, _display_,
		_write_, _writeq_, _nl_, _put_, _tab_,
		_xprompt_, _exists_, _rename_, _chdir_, _sh_,
		_shell_, _save_, _read2_, _3seeing_, _3telling_,
		_fileerrors_, _nofileerrors_, _LC_, _NOLC_, _change_chtype_,
		_op_, _prompt_, _trace_, _leash_, _debug_,
		_flags_, _all_float_, _sysp_, _sysflgs_, _recons_,
		_carith_, _unknown_, _plus_, _statistics_, _ttyflush_,
		_curlineno_, _set_, _lseq_, _expfilename_, _colour_,
		_offset_, _enable_, _plot_, _line_, _trapeze_,
		_triangle_, _fill_, _clear_, _mouse_ };

#define NPREDS		133

