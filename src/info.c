/************************************************************************
*									*
*		   C Prolog	info.c					*
*		   ========	-------					*
*									*
*  By cprolog.com, Mar 1982.					*
*  EdCAAD, Dept. of Architecture, University of Edinburgh.		*
*									*
*  Copyright (C) 2020 cprolog.com	*
*									*
************************************************************************/
#include <string.h>
#include <stdlib.h>
#include "pl.h"
#include "evalp.h"
#include "arith.h"

void ProPrintAtom(char *a_buf, ATOMP a_atom) {
	if(a_atom->atofae==a_atom) {
		sprint(a_buf, "%s", a_atom->stofae);
	} else {
		ProPrintAtom(a_buf, a_atom->atofae);
	}

}

void ProPrintFunctor(char *a_buf, FUNCTORP a_functor) {
	char v_buf[2000];
	ProPrintAtom(v_buf, a_functor->atoffe);
	sprint(a_buf, "%s", v_buf);
}

void ProPrintObj(char *a_buf, PTR a_obj, PTR v1t, PTR vt, char a_adr) {
	PTR p_var = NULL;
	PTR v_par = a_obj;
	if(IsaVar(v_par)) {
		if(a_adr=='A') {
			if(Signed(v_par) >= LCLVAR0) {
				sprint(a_buf, "%s%d", "L", (Unsigned(v_par)&ProVarNumberMask)/4);
				return;
			} else {
				sprint(a_buf, "%s%d", "G", (Unsigned(v_par)&ProVarNumberMask)/8);
				return;
			}
		}

		if(a_adr=='G') {
			p_var = FrameGlo(v_par, v1t);
		} else {
			p_var = FrameVar(v_par, v1t, vt);
		}
		while(IsaRef(v_par = VarVal(p_var))) p_var = v_par;

		if(Undef(v_par)) {
			strcpy(a_buf, "_");
			return;
		}
	}

	if(IsPrim(v_par)) {
		if(IsNumber(v_par)) {
			strcpy(a_buf, num2chars(v_par));
			return;
		}
		strcpy(a_buf, "P");
		return;
	}
	if(IsaAtomic(v_par)) {
		sprint(a_buf, "%s", ((char*) AtomP(v_par)->stofae));
		return;
	}
	if(IsaVar(v_par)) {
		strcpy(a_buf, "V");
		return;
	}

	// input term
	if(p_var) ProPrintSkel(a_buf, SkelP(v_par), MolP(p_var)->Env, vt, a_adr);
	else ProPrintSkel(a_buf, SkelP(v_par), v1t, vt, a_adr);
}

void ProPrintObjList(char *a_buf, PTR a_obj, PTR v1t, PTR vt, SKELP a_list, char a_adr) {
	PTR v_par = a_obj;
	if(IsaVar(v_par)) {
		if(a_adr=='A') {
			if(Signed(v_par) >= LCLVAR0) {
				sprint(a_buf, "%s%d", "L", (Unsigned(v_par)&ProVarNumberMask)/8);
				return;
			} else {
				sprint(a_buf, "%s%d", "G", (Unsigned(v_par)&ProVarNumberMask)/8);
				return;
			}
		}

		PTR p_var;
		if(a_adr=='G') {
			p_var = FrameGlo(v_par, v1t);
		} else {
			p_var = FrameVar(v_par, v1t, vt);
		}
		while(IsaRef(v_par = VarVal(p_var))) p_var = v_par;

		if(Undef(v_par)) {
			strcpy(a_buf, "_");
			return;
		}
	}

	if(IsPrim(v_par)) {
		if(IsNumber(v_par)) {
			strcpy(a_buf, num2chars(v_par));
			return;
		}
		strcpy(a_buf, "P");
		return;
	}
	if(IsaAtomic(v_par)) {
		sprint(a_buf, "%s", ((char*) AtomP(v_par)->stofae));
		return;
	}
	if(IsaVar(v_par)) {
		strcpy(a_buf, "V");
		return;
	}

	// input term
	ProPrintSkelList(a_buf, SkelP(v_par), v1t, vt, a_list, a_adr);
}

void ProPrintSkel(char *a_buf, SKELP a_skel, PTR v1t, PTR vt, char a_adr) {
	char v_buf[2000];
	char v_pars[2000], v_p[2000];
	ProPrintFunctor(v_buf, a_skel->Fn);

	PTR v_params = (PTR) a_skel;
	int arity = a_skel->Fn->arityoffe;
	v_pars[0] = 0;
	int is_first = 1;
	while(--arity >= 0) {
		PTR v_par = NextArg(v_params);

		if(a_skel==SkelP(v_par)) {
			strcpy(v_p, "...");
			if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
			strcat(v_pars, v_p);
			continue;
		}

		if(a_skel->Fn==listfunc) {
			ProPrintObjList(v_p, v_par, v1t, vt, a_skel, a_adr);
			if((arity==0) && (!strcmp(v_p, "[]"))) {
				continue;
			}
			if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
			strcat(v_pars, v_p);
			continue;
		}

		ProPrintObj(v_p, v_par, v1t, vt, a_adr);
		if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
		strcat(v_pars, v_p);
	}
	if(a_skel->Fn==listfunc) sprint(a_buf, "[%s]", v_pars);
	else sprint(a_buf, "%s(%s)", v_buf, v_pars);
}

void ProPrintSkelList(char *a_buf, SKELP a_skel, PTR v1t, PTR vt, SKELP a_list, char a_adr) {
	char v_buf[2000];
	char v_pars[2000], v_p[2000];
	ProPrintFunctor(v_buf, a_skel->Fn);

	PTR v_params = (PTR) a_skel;
	int arity = a_skel->Fn->arityoffe;
	v_pars[0] = 0;
	int is_first = 1;
	while(--arity >= 0) {
		PTR v_par = NextArg(v_params);

		if(a_skel==SkelP(v_par)) {
			strcpy(v_p, "...");
			if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
			strcat(v_pars, v_p);
			continue;
		}

		if(a_skel->Fn==listfunc) {
			ProPrintObjList(v_p, v_par, v1t, vt, a_list, a_adr);
			if((arity==0) && (!strcmp(v_p, "[]"))) {
				continue;
			}
			if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
			strcat(v_pars, v_p);
			continue;
		}

		ProPrintObj(v_p, v_par, v1t, vt, a_adr);
		if((arity==0) && (!strcmp(v_p, "[]"))) {
			continue;
		}
		if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
		strcat(v_pars, v_p);
	}
	if(a_skel->Fn==listfunc) sprint(a_buf, "%s", v_pars);
	else sprint(a_buf, "%s(%s)", v_buf, v_pars);
}


void ProPrintClause(char *a_buf, CLAUSEP a_clause, PTR v1t, PTR vt, char a_adr) {
	char v_pars[2000], v_p[2000];
	PTR v_head = a_clause->hdofcl;
	PTR v_params = a_clause->hdofcl;
	int arity = SkelFuncP(v_head)->arityoffe;

	v_pars[0] = 0;
	int is_first = 1;
	while(--arity >= 0) {
		PTR v_par = NextArg(v_params);

		if(a_clause==ClauseP(v_par)) {
			strcpy(v_p, "...");
			if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
			strcat(v_pars, v_p);
			continue;
		}

		ProPrintObj(v_p, v_par, v1t, vt, a_adr);
		if(is_first) { is_first=0; } else { strcat(v_pars, ","); };
		strcat(v_pars, v_p);
	}
	sprint(a_buf, "%s(%s)", ((char*) SkelFuncP(v_head)->atoffe->stofae), v_pars);
}

void ProShowSkel(SKELP a_skel, PTR v1t, PTR vt, char a_adr) {
	char v_buf[2000];
	ProPrintSkel(v_buf, a_skel, v1t, vt, a_adr);
	ProMessage("%s\n", v_buf);
}

void ProShowClause(CLAUSEP a_clause, PTR v1t, PTR vt, char a_adr) {
	char v_buf[2000];
	ProPrintClause(v_buf, a_clause, v1t, vt, a_adr);
	ProMessage("%s\n", v_buf);
}

void ProCheckClause(CLAUSEP a_clause, PTR v1t, PTR vt, char *a_text) {
	char v_buf[2000];
	ProPrintClause(v_buf, a_clause, v1t, vt, 0);
	if(strcmp(v_buf, a_text)==0) {
		ProMessage("DEBUG: %s\n", v_buf);
	}
}

int ProCheckSkelTry(SKELP a_skel, const char *a_text) {
	if(strcmp(((char*) a_skel->Fn->atoffe->stofae), a_text)==0) {
		ProMessage("DEBUG TRY: %s\n", ((char*) a_skel->Fn->atoffe->stofae));
		return 1;
	}
	return 0;
}

int ProCheckClauseTry(CLAUSEP a_clause, const char *a_text) {
	PTR v_head = a_clause->hdofcl;
	if(strcmp(((char*) SkelFuncP(v_head)->atoffe->stofae), a_text)==0) {
		ProMessage("DEBUG TRY: %s\n", ((char*) SkelFuncP(v_head)->atoffe->stofae));
		return 1;
	}
	return 0;
}
