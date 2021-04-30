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

#define _and_		 1
#define _hidden_call_	 2
#define _user_call_	 3
#define _call_		 4
#define _cut_		 5
#define _repeat_	 6
#define _abort_		 7
#define _call1_		 8
#define _yes_		 9
#define _no_		10
#define _halt_		11
#define _break_		12
#define _break_start_	13
#define _break_end_	14
#define _exit_break_	15
#define _user_exec_	16
#define _repply_	17
#define	_fail_		18
#define _kcompare_	19
#define _ncompare_	20
/*      +EQ +NE +LT +GE +GT +LE
	are reserved for =:= =/= < >= > =<
*/
#define _lseq_	       122
#define _plus_	       117
#define _succ_		26
#define _is_		27
/*	+0 +1 +2
	are reserved for is/2, is/3, is/4
*/
#define _var_		30
#define _nonvar_	31
#define _integer_	32
#define _number_	33
#define _primitive_	34
#define _db_reference_	35
#define _atomic_	36
#define _atom_		37
#define	_compound_	38
#define _compare_	39
#define _tcompare_	40
/*	+ EQ +NE +LT +GE +GT +LE
	are reserved for == \== @< @>= @> @=<
*/
#define _name_		46
#define _functor_	47
#define _arg_		48
#define _univ_		49
#define _z1_assert_	50
#define _a1_assert_	51
#define _z2_assert_	52
#define _a2_assert_	53
#define _recordz_	54
#define _recorda_	55
#define _assertr_	56
#define _instance_	57
#define _erase_		58
#define _erased_	59
#define _clause_	60
/* reserved for loop	61	*/
#define _recorded_	62
/* reserved for loop	63	*/
#define _catom_		64
/* reserved for loop	65	*/
#define _cfunctor_	66
/* reserved for loop	67	*/
#define _abolish_	68
#define _is_op_		69
#define _see_		70
#define _seeing_	71
#define _2seeing_	72
#define _3seeing_	98
#define _seen_		73
#define _tell_		74
#define _append_	75
#define _telling_	76
#define _2telling_	77
#define _3telling_	99
#define _told_		78
#define _close_		79
#define	_ttyflush_	119
#define _read_		80
#define _read2_		97
#define _get0_		81
#define _get_		82
#define _skip_		83
#define _curlineno_	120
#define _display_	84
#define _write_		85
#define _writeq_	86
#define _nl_		87
#define _put_		88
#define _tab_		89
#define _xprompt_	90
#define _expfilename_	123
#define _exists_	91
#define _rename_	92
#define _chdir_		93
#define _sh_		94
#define _shell_		95
#define _save_		96
/* 97-99 used by late additions */
#define _fileerrors_	100
#define _nofileerrors_	101
#define _LC_		102
#define _NOLC_		103
#define _change_chtype_	104
#define _op_		105
#define _prompt_	106
#define _trace_		107
#define _leash_		108
#define _debug_		109
#define _flags_		110
#define _all_float_	111
#define _sysp_		112
#define _sysflgs_	113
#define _recons_	114
#define _carith_	115
#define _unknown_	116
#define	_statistics_	118
#define _set_		121
#define _colour_	124
#define	_offset_	125
#define	_enable_	126
#define	_plot_		127
#define	_line_		128
#define	_trapeze_	129
#define	_triangle_	130
#define	_fill_		131
#define	_clear_		132
#define	_mouse_		133

#define NPREDS		133

