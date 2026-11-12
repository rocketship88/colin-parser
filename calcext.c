/*
 * calcext.c -- the Calc / colon module (colon-1.0.tm) as a Tcl C extension.
 *
 * Build: one binary per Tcl version (8.6 or 9.x), always with stubs (-DUSE_TCL_STUBS), so the
 * same source works for both; the version is taken from TCL_MAJOR_VERSION in tcl.h.
 *
 *  Linux / Unix, building against Tcl's source tree (stub code compiled straight in):
 *    gcc -shared -fPIC -O2 -DUSE_TCL_STUBS -I<tcl>/generic -I<tcl>/unix -I<tcl>/libtommath \
 *        calcext.c <tcl>/generic/tclStubLib.c <tcl>/generic/tclTomMathStubLib.c -o calcext.so
 *    (tclStubLib.c and tclTomMathStubLib.c are the two stub-table sources; with an installed Tcl
 *     you can link its libtclstub instead of compiling them.)
 *
 *  Windows, Visual Studio command prompt (x64), against an installed / merged Tcl 9 tree:
 *    cl /O2 /Ob2 /Oi /Ot /GL /DUSE_TCL_STUBS -I<tcl>\include calcext.c ^
 *       /link -dll <tcl>\lib\tclstub.lib /OUT:calcext.dll
 *    e.g.  cl /O2 /Ob2 /Oi /Ot /GL /DUSE_TCL_STUBS -IA:\aaa\tcltk-merged-release\include calcext.c ^
 *          /link -dll A:\aaa\tcltk-merged-release\lib\tclstub.lib /OUT:calcext.dll
 *    -I must point at the folder that holds tcl.h; there must be no extra "C:" in front of the drive.
 *    For Tcl 8.6 use that tree's include and its tclstub86.lib (or tclstub.lib) instead; the 64-bit
 *    prompt is needed for a 64-bit tclsh, and the DLL must match the tclsh's bitness.
 *    (A /DTCL9 define is not used by this file.)
 *
 * Use (the second argument is optional when the file is named calcext.so / calcext.dll):
 *   load ./calcext.so
 *   load ./calcext.dll
 * If the file is renamed (calcext-tcl9.1.so, say) give the init name:  load ./calcext-tcl9.1.so Calcext
 * Loading the same file a second time in an interpreter does nothing; a changed build has to be
 * loaded under another file name, or in a new process.  It also does  package provide colon 1.0.
 *
 * The code follows the Tcl module proc by proc.  Every proc became a C command with the
 * same name and the same arguments, and error messages and generated TAL are identical.
 */
#include <tcl.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#if TCL_MAJOR_VERSION < 9
typedef int Tcl_Size;
#endif

/* ------------------------------------------------------------------------------------
 * Per interpreter state
 * ---------------------------------------------------------------------------------- */
typedef struct CalcState {
    Tcl_Obj *v_inprocFlag;     /* ::Calc::inprocFlag */
    Tcl_Obj *v_tokens;         /* ::Calc::tokens */
    Tcl_Obj *v_tokpos;         /* ::Calc::tokpos */
    Tcl_Obj *v_depth;          /* ::Calc::depth */
    Tcl_Obj *v_strictFlag;     /* ::Calc::strictFlag */
    Tcl_Obj *v_labelcount;     /* ::labelcount */
    Tcl_Obj *nameRe;           /* ^[[:alpha:]_][[:alnum:]_]*$ */
    Tcl_Obj *emptyObj;
    Tcl_Obj *zeroObj;
    Tcl_Obj *colonObj;
    Tcl_Obj *peepRe;           /* peephole regexp for stores inside a proc */
    Tcl_Obj *reP1;
    Tcl_Obj *reP1b;
    Tcl_Obj *reComment;
    Tcl_Obj *reP2;
    Tcl_Obj *reP3a;
    Tcl_Obj *reP3ab;
    Tcl_Obj *reSuffix;
    Tcl_Obj *reP3;
    Tcl_Obj *reP3c;
    Tcl_Obj *reLoad;
    Tcl_Obj *oneObj;
    Tcl_Obj *v_cache;          /* ::Calc::cache (array) */
    Tcl_ObjCmdProc *assembleProc;   /* ::tcl::unsupported::assemble, found on first use */
    void *assembleCD;
    Tcl_Obj *assembleName;
    Tcl_Obj *v_scratch;        /* ::Calc::_defaultVal, receives [info default] */
    Tcl_Obj *v_inprec;         /* ::Calc::inprec (array) */
    Tcl_Obj *v_precode;        /* ::Calc::precode (array) */
    Tcl_Obj *v_incode;         /* ::Calc::incode (array) */
    Tcl_Obj *v_preprec;        /* ::Calc::preprec */
    Tcl_Obj *v_nativeFunc;     /* ::Calc::nativeFunc (array) */
} CalcState;

static Tcl_Obj *Parse(struct CalcState *st, Tcl_Interp *interp, double minPrec);

/* Tcl 9 command procs take a Tcl_Size objc (Tcl_ObjCmdProc2).  tcl.h only picks Tcl_CreateObjCommand2 by itself
 * with a C11 compiler (_Generic), which older MSVC modes lack, so name it explicitly. */
#if TCL_MAJOR_VERSION >= 9
#   define CALC_CREATE Tcl_CreateObjCommand2
#else
#   define CALC_CREATE Tcl_CreateObjCommand
#endif
#define LOCAL_NAME(var, str) do { var = Tcl_NewStringObj(str, -1); Tcl_IncrRefCount(var); } while (0)

/* ------------------------------------------------------------------------------------
 * small helpers
 * ---------------------------------------------------------------------------------- */

/* read a Tcl variable (qualified name), error message left in the interp */
static Tcl_Obj *GetVar(Tcl_Interp *interp, Tcl_Obj *name) {
    return Tcl_ObjGetVar2(interp, name, NULL, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
}

static int SetVarInt(Tcl_Interp *interp, Tcl_Obj *name, long v) {
    return Tcl_ObjSetVar2(interp, name, NULL, Tcl_NewLongObj(v), TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG) ? TCL_OK : TCL_ERROR;
}

/* Parser state.  Held in the Calc namespace variables of the Tcl module for now. */
static int GetTokpos(CalcState *st, Tcl_Interp *interp, long *v) {
    Tcl_Obj *o = GetVar(interp, st->v_tokpos);
    if (o == NULL) return TCL_ERROR;
    return Tcl_GetLongFromObj(interp, o, v);
}
static int SetTokpos(CalcState *st, Tcl_Interp *interp, long v) { return SetVarInt(interp, st->v_tokpos, v); }

/* lindex $tokens $idx : the empty string outside the list */
static Tcl_Obj *TokenAt(CalcState *st, Tcl_Interp *interp, long idx) {
    Tcl_Obj *toks = GetVar(interp, st->v_tokens), *e = NULL;
    if (toks == NULL) return NULL;
    if (idx >= 0 && Tcl_ListObjIndex(interp, toks, (Tcl_Size) idx, &e) != TCL_OK) return NULL;
    return e != NULL ? e : st->emptyObj;
}

/* regexp {^[[:alpha:]_][[:alnum:]_]*$} : plain ASCII is decided here, anything else by Tcl */
static int IsName(CalcState *st, Tcl_Interp *interp, Tcl_Obj *obj) {
    Tcl_Size len, i;
    const char *p = Tcl_GetStringFromObj(obj, &len);
    int ascii = 1;
    if (len == 0) return 0;
    for (i = 0; i < len; i++) {
        unsigned char c = (unsigned char) p[i];
        if (c >= 0x80) { ascii = 0; break; }
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || (i > 0 && c >= '0' && c <= '9'))) return 0;
    }
    if (ascii) return 1;
    {
        Tcl_RegExp re = Tcl_GetRegExpFromObj(interp, st->nameRe, TCL_REG_ADVANCED);
        return re != NULL && Tcl_RegExpExecObj(interp, re, obj, 0, 0, 0) > 0;
    }
}

/* error "message": sets the result and returns TCL_ERROR */
static int CalcErr(Tcl_Interp *interp, Tcl_Obj *msg) { Tcl_SetObjResult(interp, msg); return TCL_ERROR; }
#define CALC_ERROR(interp, ...) CalcErr((interp), Tcl_ObjPrintf(__VA_ARGS__))

/* ------------------------------------------------------------------------------------
 * incrCode {name delta}
 * TAL for name += delta (delta is +1 or -1); leaves the new value on the stack.
 * Inside a proc the variable is a real local slot (incrImm), otherwise by name.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *IncrCode(CalcState *st, Tcl_Interp *interp, const char *name, const char *delta) {
    int flag;
    Tcl_Obj *fv = GetVar(interp, st->v_inprocFlag), *r;
    if (fv == NULL || Tcl_GetBooleanFromObj(interp, fv, &flag) != TCL_OK) return NULL;
    if (flag) r = Tcl_ObjPrintf("incrImm %s %s; ", name, delta);
    else r = Tcl_ObjPrintf("push %s; incrStkImm %s; ", name, delta);
    Tcl_IncrRefCount(r);
    return r;
}

static int CalcIncrCode(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "name delta");
        return TCL_ERROR;
    }
    r = IncrCode((CalcState *) cd, interp, Tcl_GetString(objv[1]), Tcl_GetString(objv[2]));
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * patternText pat
 * text of a pattern, for error messages: (a,(b,c))
 * ---------------------------------------------------------------------------------- */
static int PatternText(Tcl_Interp *interp, Tcl_Obj *pat, Tcl_Obj **res) {
    Tcl_Size n, i;
    Tcl_Obj **e;
    if (Tcl_ListObjGetElements(interp, pat, &n, &e) != TCL_OK) return TCL_ERROR;
    if (n > 0 && strcmp(Tcl_GetString(e[0]), "n") == 0) {
        *res = (n > 1) ? e[1] : Tcl_NewObj();
        Tcl_IncrRefCount(*res);
        return TCL_OK;
    }
    Tcl_Obj *out = Tcl_NewStringObj("(", 1);
    Tcl_IncrRefCount(out);
    for (i = 1; i < n; i++) {
        Tcl_Obj *part;
        if (i > 1) Tcl_AppendToObj(out, ",", 1);
        if (PatternText(interp, e[i], &part) != TCL_OK) { Tcl_DecrRefCount(out); return TCL_ERROR; }
        Tcl_AppendObjToObj(out, part);
        Tcl_DecrRefCount(part);
    }
    Tcl_AppendToObj(out, ")", 1);
    *res = out;
    return TCL_OK;
}

static int CalcPatternText(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *res;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "pat"); return TCL_ERROR; }
    if (PatternText(interp, objv[1], &res) != TCL_OK) return TCL_ERROR;
    Tcl_SetObjResult(interp, res);
    Tcl_DecrRefCount(res);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * run a Tcl core command from C and keep its result (caller releases it)
 * ---------------------------------------------------------------------------------- */
static int EvalKeep(Tcl_Interp *interp, int objc, Tcl_Obj **objv, Tcl_Obj **res) {
    int i, rc;
    for (i = 0; i < objc; i++) Tcl_IncrRefCount(objv[i]);
    rc = Tcl_EvalObjv(interp, objc, objv, 0);
    for (i = 0; i < objc; i++) Tcl_DecrRefCount(objv[i]);
    if (rc == TCL_OK && res != NULL) {
        *res = Tcl_GetObjResult(interp);
        Tcl_IncrRefCount(*res);
    }
    return rc;
}

#define S(x) Tcl_NewStringObj((x), -1)
/* run a Tcl command given as n words (Tcl_Obj *), result kept in *res (may be NULL) */
static int RunV(Tcl_Interp *interp, Tcl_Obj **res, int n, ...) {
    Tcl_Obj *w[16];
    va_list ap;
    int i;
    va_start(ap, n);
    for (i = 0; i < n; i++) w[i] = va_arg(ap, Tcl_Obj *);
    va_end(ap);
    return EvalKeep(interp, n, w, res);
}

/* ------------------------------------------------------------------------------------
 * debug label arglist body newbody
 * Writes the original and the transformed body to two files under $::env(TEMP) and
 * opens them in an external diff program.
 * ---------------------------------------------------------------------------------- */
static int CalcDebug(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *label, *arglist, *body, *newbody, *tmp, *f[2], *r, *io;
    int k, rc;
    if (objc != 5) { Tcl_WrongNumArgs(interp, 1, objv, "label arglist body newbody"); return TCL_ERROR; }
    label = objv[1]; arglist = objv[2]; body = objv[3]; newbody = objv[4];
    f[0] = f[1] = NULL;
    for (k = 0; k < 2; k++) {
        /* file delete [set f [file normalize [file join $::env(TEMP) calc-temp-N.tcl]]] */
        tmp = Tcl_ObjGetVar2(interp, S("::env"), S("TEMP"), TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
        if (tmp == NULL) goto fail;
        if (RunV(interp, &r, 4, S("file"), S("join"), tmp, S(k == 0 ? "calc-temp-1.tcl" : "calc-temp-2.tcl")) != TCL_OK) goto fail;
        rc = RunV(interp, &f[k], 3, S("file"), S("normalize"), r);
        Tcl_DecrRefCount(r);
        if (rc != TCL_OK) goto fail;
        if (RunV(interp, NULL, 3, S("file"), S("delete"), f[k]) != TCL_OK) goto fail;
    }
    for (k = 0; k < 2; k++) {
        /* set io [open $f w]; puts $io "original|transformed $label $arglist"; puts $io $body; close $io */
        if (RunV(interp, &io, 3, S("open"), f[k], S("w")) != TCL_OK) goto fail;
        rc = RunV(interp, NULL, 3, S("puts"), io,
                  Tcl_ObjPrintf("%s %s %s", k == 0 ? "original" : "transformed", Tcl_GetString(label), Tcl_GetString(arglist)));
        if (rc == TCL_OK) rc = RunV(interp, NULL, 3, S("puts"), io, k == 0 ? body : newbody);
        if (rc == TCL_OK) rc = RunV(interp, NULL, 2, S("close"), io);
        Tcl_DecrRefCount(io);
        if (rc != TCL_OK) goto fail;
    }
    if (RunV(interp, NULL, 4, S("exec"), S("C:/Program Files (x86)/Beyond Compare 2/BC2.exe"), f[0], f[1]) != TCL_OK) goto fail;
    if (RunV(interp, NULL, 3, S("file"), S("delete"), f[0]) != TCL_OK) goto fail;
    if (RunV(interp, NULL, 3, S("file"), S("delete"), f[1]) != TCL_OK) goto fail;
    Tcl_DecrRefCount(f[0]); Tcl_DecrRefCount(f[1]);
    Tcl_ResetResult(interp);
    return TCL_OK;
fail:
    if (f[0]) Tcl_DecrRefCount(f[0]);
    if (f[1]) Tcl_DecrRefCount(f[1]);
    return TCL_ERROR;
}

/* ------------------------------------------------------------------------------------
 * AllProcsRecursive
 * every proc in every namespace, fully qualified, sorted and unique
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *AllProcsRecursive(Tcl_Interp *interp) {
    Tcl_Obj *out = Tcl_NewListObj(0, NULL);
    Tcl_Obj *pending = Tcl_NewListObj(0, NULL);
    Tcl_IncrRefCount(out);
    Tcl_IncrRefCount(pending);
    Tcl_ListObjAppendElement(NULL, pending, Tcl_NewStringObj("::", -1));
    int rc = TCL_OK;
    Tcl_Size np;
    while (rc == TCL_OK && Tcl_ListObjLength(interp, pending, &np) == TCL_OK && np > 0) {
        Tcl_Obj *ns, *r, *w[3];
        Tcl_ListObjIndex(interp, pending, 0, &ns);
        Tcl_IncrRefCount(ns);
        Tcl_ListObjReplace(interp, pending, 0, 1, 0, NULL);

        w[0] = Tcl_NewStringObj("info", -1);
        w[1] = Tcl_NewStringObj("procs", -1);
        w[2] = Tcl_ObjPrintf("%s::*", Tcl_GetString(ns));
        if ((rc = EvalKeep(interp, 3, w, &r)) == TCL_OK) {
            Tcl_ListObjAppendList(interp, out, r);
            Tcl_DecrRefCount(r);
        }
        if (rc == TCL_OK && strcmp(Tcl_GetString(ns), "::") == 0) {
            w[0] = Tcl_NewStringObj("info", -1);
            w[1] = Tcl_NewStringObj("procs", -1);
            w[2] = Tcl_NewStringObj("::*", -1);
            if ((rc = EvalKeep(interp, 3, w, &r)) == TCL_OK) {
                Tcl_ListObjAppendList(interp, out, r);
                Tcl_DecrRefCount(r);
            }
        }
        if (rc == TCL_OK) {
            w[0] = Tcl_NewStringObj("namespace", -1);
            w[1] = Tcl_NewStringObj("children", -1);
            w[2] = ns;
            if ((rc = EvalKeep(interp, 3, w, &r)) == TCL_OK) {
                Tcl_ListObjAppendList(interp, pending, r);
                Tcl_DecrRefCount(r);
            }
        }
        Tcl_DecrRefCount(ns);
    }
    if (rc == TCL_OK) {
        Tcl_Obj *s[3], *r;
        s[0] = Tcl_NewStringObj("lsort", -1);
        s[1] = Tcl_NewStringObj("-unique", -1);
        s[2] = out;
        if ((rc = EvalKeep(interp, 3, s, &r)) == TCL_OK) {
            Tcl_DecrRefCount(out);
            out = r;                      /* the sorted list, refcount 1 */
        }
    }
    Tcl_DecrRefCount(pending);
    if (rc != TCL_OK) { Tcl_DecrRefCount(out); return NULL; }
    return out;
}

static int CalcAllProcsRecursive(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 1) { Tcl_WrongNumArgs(interp, 1, objv, NULL); return TCL_ERROR; }
    if ((r = AllProcsRecursive(interp)) == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * [string is CLASS ?-strict? obj] through Tcl itself, for the cases where the exact Tcl
 * (8.6 versus 9) rules for numbers matter.  A cheap pre-filter avoids most calls.
 * ---------------------------------------------------------------------------------- */
static int StrIs(Tcl_Interp *interp, const char *cls, int strict, Tcl_Obj *obj, int *result) {
    Tcl_Obj *w[5], *r;
    int n = 0, rc;
    w[n++] = S("string"); w[n++] = S("is"); w[n++] = S(cls);
    if (strict) w[n++] = S("-strict");
    w[n++] = obj;
    rc = EvalKeep(interp, n, w, &r);
    if (rc != TCL_OK) return TCL_ERROR;
    rc = Tcl_GetBooleanFromObj(interp, r, result);
    Tcl_DecrRefCount(r);
    return rc;
}

/* string trim with Tcl's default character set (whitespace plus U+0000 and Unicode spaces) */
static const char *const trimSet[] = {
    "\xC0\x80", "\xC2\x85", "\xC2\xA0", "\xE1\x9A\x80", "\xE1\xA0\x8E",
    "\xE2\x80\x80", "\xE2\x80\x81", "\xE2\x80\x82", "\xE2\x80\x83", "\xE2\x80\x84", "\xE2\x80\x85",
    "\xE2\x80\x86", "\xE2\x80\x87", "\xE2\x80\x88", "\xE2\x80\x89", "\xE2\x80\x8A", "\xE2\x80\x8B",
    "\xE2\x80\xA8", "\xE2\x80\xA9", "\xE2\x80\xAF", "\xE2\x81\x9F", "\xE2\x81\xA0", "\xE3\x80\x80",
    "\xEF\xBB\xBF", NULL
};

/* length in bytes of the trim character that starts at p (limit e), or 0 */
static int TrimAt(const char *p, const char *e) {
    unsigned char c = (unsigned char) *p;
    int i;
    if (c == ' ' || (c >= 0x09 && c <= 0x0D)) return 1;
    if (c < 0xC0) return 0;
    for (i = 0; trimSet[i] != NULL; i++) {
        size_t l = strlen(trimSet[i]);
        if ((size_t) (e - p) >= l && memcmp(p, trimSet[i], l) == 0) return (int) l;
    }
    return 0;
}

/* trims [s, e) in place by moving the two pointers */
static void TrimRange(const char **sp, const char **ep) {
    const char *s = *sp, *e = *ep;
    int n;
    while (s < e && (n = TrimAt(s, e)) > 0) s += n;
    while (e > s) {
        /* a trim character ends at e: look back up to three bytes */
        int k, hit = 0;
        for (k = 1; k <= 3 && e - k >= s; k++) {
            if (TrimAt(e - k, e) == k) { hit = k; break; }
        }
        if (!hit) break;
        e -= hit;
    }
    *sp = s; *ep = e;
}

/* ------------------------------------------------------------------------------------
 * tokenise input
 * Splits an expression into operator and operand tokens; operands are trimmed.
 * Scientific notation split by the operator scan (3.0e - 33) is joined again.
 * ---------------------------------------------------------------------------------- */
/* length of the operator that starts at p (0 if none); the longest match wins */
static int OpLen(const char *p, const char *e) {
    char c = p[0], d = (p + 1 < e) ? p[1] : '\0';
    switch (c) {
    case '+': return d == '+' ? 2 : 1;
    case '-': return (d == '-' || d == '>') ? 2 : 1;
    case '*': return d == '*' ? 2 : 1;
    case '>': return (d == '>' || d == '=') ? 2 : 1;
    case '<': return (d == '<' || d == '=') ? 2 : 1;
    case '!': return d == '=' ? 2 : 1;
    case '=': return (d == '=' || d == '>') ? 2 : 1;
    case '&': return d == '&' ? 2 : 1;
    case '|': return d == '|' ? 2 : 1;
    case ':': return d == ':' ? 2 : 1;
    case '%': case '/': case '^': case '?': case ',': case '~': case '(': case ')': return 1;
    default: return 0;
    }
}

/* ^[0-9]*\.?[0-9]+[eE]$ */
static int IsSciPrefix(const char *s, Tcl_Size n) {
    Tcl_Size i = 0, digitsAfter = 0, dots = 0;
    if (n < 2 || (s[n - 1] != 'e' && s[n - 1] != 'E')) return 0;
    n--;
    for (i = 0; i < n; i++) {
        if (s[i] >= '0' && s[i] <= '9') digitsAfter++;
        else if (s[i] == '.' && dots == 0) { dots = 1; digitsAfter = 0; }
        else return 0;
    }
    return digitsAfter > 0;
}

static int Tokenise(Tcl_Interp *interp, Tcl_Obj *input, Tcl_Obj **result) {
    Tcl_Size len, i = 0, nOut;
    const char *str = Tcl_GetStringFromObj(input, &len);
    const char *end = str + len, *pos = str;
    Tcl_Obj *out = Tcl_NewListObj(0, NULL);
    int needsMerge = 0;
    Tcl_IncrRefCount(out);

    while (i < len) {
        int ol = OpLen(str + i, end);
        if (ol == 0) { i++; continue; }
        /* operand text between pos and the operator */
        {
            const char *s = pos, *e = str + i;
            TrimRange(&s, &e);
            if (e > s) {
                Tcl_Obj *prev = Tcl_NewStringObj(s, (Tcl_Size) (e - s));
                char last = e[-1];
                Tcl_ListObjAppendElement(NULL, out, prev);
                if (last == 'e' || last == 'E') {
                    Tcl_Obj *t = Tcl_NewStringObj(s, (Tcl_Size) (e - s));
                    int isd;
                    Tcl_AppendToObj(t, "0", 1);
                    Tcl_IncrRefCount(t);
                    if (StrIs(interp, "double", 1, t, &isd) != TCL_OK) { Tcl_DecrRefCount(t); Tcl_DecrRefCount(out); return TCL_ERROR; }
                    Tcl_DecrRefCount(t);
                    if (isd) needsMerge = 1;
                }
            }
        }
        Tcl_ListObjAppendElement(NULL, out, Tcl_NewStringObj(str + i, ol));
        i += ol;
        pos = str + i;
    }
    {
        const char *s = pos, *e = end;
        TrimRange(&s, &e);
        if (e > s) Tcl_ListObjAppendElement(NULL, out, Tcl_NewStringObj(s, (Tcl_Size) (e - s)));
    }

    if (!needsMerge) { *result = out; return TCL_OK; }

    /* merge scientific notation tokens: 3.0e - 33 -> 3.0e-33 */
    {
        Tcl_Obj **el, *merged = Tcl_NewListObj(0, NULL);
        Tcl_Size j = 0, tl;
        Tcl_ListObjGetElements(NULL, out, &nOut, &el);
        while (j < nOut) {
            const char *tok = Tcl_GetStringFromObj(el[j], &tl);
            int merge = 0;
            if (IsSciPrefix(tok, tl) && j + 2 < nOut) {
                const char *op = Tcl_GetString(el[j + 1]);
                if (op[0] != '\0' && op[1] == '\0' && (op[0] == '-' || op[0] == '+')) {
                    int dig;
                    if (StrIs(interp, "digit", 0, el[j + 2], &dig) != TCL_OK) { Tcl_DecrRefCount(merged); Tcl_DecrRefCount(out); return TCL_ERROR; }
                    merge = dig;
                }
            }
            if (merge) {
                Tcl_Obj *m = Tcl_DuplicateObj(el[j]);
                Tcl_AppendObjToObj(m, el[j + 1]);
                Tcl_AppendObjToObj(m, el[j + 2]);
                Tcl_ListObjAppendElement(NULL, merged, m);
                j += 3;
            } else {
                Tcl_ListObjAppendElement(NULL, merged, el[j]);
                j++;
            }
        }
        Tcl_DecrRefCount(out);
        Tcl_IncrRefCount(merged);
        *result = merged;
    }
    return TCL_OK;
}

static int CalcTokenise(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *res;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "input"); return TCL_ERROR; }
    if (Tokenise(interp, objv[1], &res) != TCL_OK) return TCL_ERROR;
    Tcl_SetObjResult(interp, res);
    Tcl_DecrRefCount(res);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * helpers for the flags and counters that the Tcl module kept in variables
 * ---------------------------------------------------------------------------------- */
/* if {$flag} for a namespace variable that has to exist */
static int GetFlag(CalcState *st, Tcl_Interp *interp, Tcl_Obj *name, int *flag) {
    Tcl_Obj *o = GetVar(interp, name);
    return o != NULL && Tcl_GetBooleanFromObj(interp, o, flag) == TCL_OK ? TCL_OK : TCL_ERROR;
}
/* info exists v && $v */
static int GetFlagIfSet(CalcState *st, Tcl_Interp *interp, Tcl_Obj *name, int *flag) {
    Tcl_Obj *o = Tcl_ObjGetVar2(interp, name, NULL, TCL_GLOBAL_ONLY);
    *flag = 0;
    if (o == NULL) return TCL_OK;
    return Tcl_GetBooleanFromObj(interp, o, flag);
}
/* incr ::labelcount */
static int NextLabel(CalcState *st, Tcl_Interp *interp, long *n) {
    Tcl_Obj *o = Tcl_ObjGetVar2(interp, st->v_labelcount, NULL, TCL_GLOBAL_ONLY);
    long v = 0;
    if (o != NULL && Tcl_GetLongFromObj(interp, o, &v) != TCL_OK) return TCL_ERROR;
    v++;
    *n = v;
    return SetVarInt(interp, st->v_labelcount, v);
}

/* ------------------------------------------------------------------------------------
 * destructure pat
 * The value to take apart is on top of the stack; it is still there afterwards.
 * Returns the TAL, a new object (refcount 1), or NULL on error.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *Destructure(CalcState *st, Tcl_Interp *interp, Tcl_Obj *pat) {
    Tcl_Size n, i;
    Tcl_Obj **e, *out;
    int inproc, strict;
    if (Tcl_ListObjGetElements(interp, pat, &n, &e) != TCL_OK) return NULL;
    if (GetFlag(st, interp, st->v_inprocFlag, &inproc) != TCL_OK) return NULL;
    {
        const char *kind = n > 0 ? Tcl_GetString(e[0]) : "";
        const char *first = n > 1 ? Tcl_GetString(e[1]) : "";
        if (strcmp(kind, "n") == 0) {
            Tcl_Obj *r;
            if (strcmp(first, "_") == 0) r = Tcl_NewObj();
            else if (inproc) r = Tcl_ObjPrintf("store %s; ", first);
            else r = Tcl_ObjPrintf("push %s; over 1; storeStk; pop; ", first);
            Tcl_IncrRefCount(r);
            return r;
        }
    }
    out = Tcl_NewObj();
    Tcl_IncrRefCount(out);
    if (GetFlagIfSet(st, interp, st->v_strictFlag, &strict) != TCL_OK) { Tcl_DecrRefCount(out); return NULL; }
    if (strict) {
        long lab;
        Tcl_Obj *txt;
        long want = (long) n - 1;
        if (NextLabel(st, interp, &lab) != TCL_OK) { Tcl_DecrRefCount(out); return NULL; }
        if (PatternText(interp, pat, &txt) != TCL_OK) { Tcl_DecrRefCount(out); return NULL; }
        Tcl_AppendPrintfToObj(out, "dup; listLength; push %ld; neq; jumpFalse tupok%ld; ", want, lab);
        Tcl_AppendPrintfToObj(out, "push error; push {Calc: tuple mismatch: pattern %s expects %ld, data has }; ", Tcl_GetString(txt), want);
        Tcl_AppendPrintfToObj(out, "over 2; listLength; push { (use -> to allow a mismatch)}; strcat 3; invokeStk 2; pop; label tupok%ld; ", lab);
        Tcl_DecrRefCount(txt);
    }
    for (i = 1; i < n; i++) {
        Tcl_Obj *kid = e[i];
        long idx = (long) (i - 1);
        const char *ks = Tcl_GetString(kid);
        Tcl_Size kn;
        Tcl_Obj **ke;
        const char *k0;
        if (strcmp(ks, "n _") == 0) continue;
        if (Tcl_ListObjGetElements(interp, kid, &kn, &ke) != TCL_OK) { Tcl_DecrRefCount(out); return NULL; }
        k0 = kn > 0 ? Tcl_GetString(ke[0]) : "";
        if (strcmp(k0, "n") == 0 && !inproc) {
            Tcl_AppendPrintfToObj(out, "push %s; over 1; listIndexImm %ld; storeStk; pop; ", kn > 1 ? Tcl_GetString(ke[1]) : "", idx);
        } else if (strcmp(k0, "n") == 0) {
            Tcl_AppendPrintfToObj(out, "dup; listIndexImm %ld; store %s; pop; ", idx, kn > 1 ? Tcl_GetString(ke[1]) : "");
        } else {
            Tcl_Obj *sub = Destructure(st, interp, kid);
            if (sub == NULL) { Tcl_DecrRefCount(out); return NULL; }
            Tcl_AppendPrintfToObj(out, "dup; listIndexImm %ld; %spop; ", idx, Tcl_GetString(sub));
            Tcl_DecrRefCount(sub);
        }
    }
    return out;
}

static int CalcDestructure(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "pat"); return TCL_ERROR; }
    r = Destructure((CalcState *) cd, interp, objv[1]);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * [parse $prec] : calls the parser.  Returns the TAL (refcount 1) or NULL on error.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *CallParse(CalcState *st, Tcl_Interp *interp, Tcl_Obj *prec) {
    double p;
    if (Tcl_GetDoubleFromObj(interp, prec, &p) != TCL_OK) return NULL;
    return Parse(st, interp, p);
}

/* ------------------------------------------------------------------------------------
 * parseFuncArgs funcname nativeInfo
 * Parses zero or more arguments of a function call, separated by commas and ended by ")".
 * Returns the TAL that evaluates the arguments and calls the function.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *ParseFuncArgs(CalcState *st, Tcl_Interp *interp, Tcl_Obj *funcname, Tcl_Obj *nativeInfo) {
    long pos, argNum = 1;
    Tcl_Obj *token, *opcodes = Tcl_NewObj();
    Tcl_IncrRefCount(opcodes);
    if (GetTokpos(st, interp, &pos) != TCL_OK || (token = TokenAt(st, interp, pos)) == NULL) goto fail;
    Tcl_IncrRefCount(token);
    for (;;) {
        const char *ts = Tcl_GetString(token);
        if (strcmp(ts, ")") == 0) {
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) goto failtok;
            if (Tcl_GetString(nativeInfo)[0] != '\0') {
                Tcl_Size ln;
                Tcl_Obj **le;
                long expected, actual = argNum - 1;
                const char *instr;
                if (Tcl_ListObjGetElements(interp, nativeInfo, &ln, &le) != TCL_OK) goto failtok;
                instr = ln > 0 ? Tcl_GetString(le[0]) : "";
                if (ln < 2) { Tcl_SetObjResult(interp, Tcl_NewStringObj("can't use empty string as operand of \">\"", -1)); goto failtok; }
                if (Tcl_GetLongFromObj(interp, le[1], &expected) != TCL_OK) goto failtok;
                if (expected > 0) {
                    if (actual != expected) {
                        CALC_ERROR(interp, "Calc: %s expects exactly %ld args, got %ld", Tcl_GetString(funcname), expected, actual);
                        goto failtok;
                    }
                } else {
                    long minArgs = expected < 0 ? -expected : expected;
                    if (actual < minArgs) {
                        CALC_ERROR(interp, "Calc: %s expects at least %ld args, got %ld", Tcl_GetString(funcname), minArgs, actual);
                        goto failtok;
                    }
                }
                if (expected < 0) Tcl_AppendPrintfToObj(opcodes, "%s %ld; ", instr, actual);
                else Tcl_AppendPrintfToObj(opcodes, "%s; ", instr);
            } else {
                Tcl_AppendPrintfToObj(opcodes, "invokeStk %ld; ", argNum);
            }
            Tcl_DecrRefCount(token);
            return opcodes;
        }
        {
            Tcl_Obj *sub = CallParse(st, interp, st->zeroObj);
            if (sub == NULL) goto failtok;
            Tcl_AppendObjToObj(opcodes, sub);
            Tcl_DecrRefCount(sub);
        }
        argNum++;
        Tcl_DecrRefCount(token);
        if (GetTokpos(st, interp, &pos) != TCL_OK || (token = TokenAt(st, interp, pos)) == NULL) goto fail;
        Tcl_IncrRefCount(token);
        ts = Tcl_GetString(token);
        if (strcmp(ts, ",") == 0) {
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) goto failtok;
        } else if (strcmp(ts, ")") != 0) {
            CALC_ERROR(interp, "Calc: expected ')' or ',' but found '%s'", ts);
            goto failtok;
        }
    }
failtok:
    Tcl_DecrRefCount(token);
fail:
    Tcl_DecrRefCount(opcodes);
    return NULL;
}

static int CalcParseFuncArgs(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 3) { Tcl_WrongNumArgs(interp, 1, objv, "funcname nativeInfo"); return TCL_ERROR; }
    r = ParseFuncArgs((CalcState *) cd, interp, objv[1], objv[2]);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * parseTernary
 * The "?" of an if-then-else was just seen.  Returns the TAL that tests the previous
 * condition and then evaluates the matching branch.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *ParseTernary(CalcState *st, Tcl_Interp *interp) {
    long lab, pos;
    Tcl_Obj *colonPrec, *opcodes, *sub, *token;
    colonPrec = Tcl_ObjGetVar2(interp, st->v_inprec, st->colonObj, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
    if (NextLabel(st, interp, &lab) != TCL_OK) return NULL;
    if (colonPrec == NULL) return NULL;
    opcodes = Tcl_ObjPrintf("jumpFalse else%ld; ", lab);
    Tcl_IncrRefCount(opcodes);
    if ((sub = CallParse(st, interp, colonPrec)) == NULL) goto fail;
    Tcl_AppendObjToObj(opcodes, sub);
    Tcl_DecrRefCount(sub);

    if (GetTokpos(st, interp, &pos) != TCL_OK || (token = TokenAt(st, interp, pos)) == NULL) goto fail;
    if (strcmp(Tcl_GetString(token), ":") != 0) {
        CALC_ERROR(interp, "Calc: expected ':' but found '%s'", Tcl_GetString(token));
        goto fail;
    }
    if (SetTokpos(st, interp, pos + 1) != TCL_OK) goto fail;

    Tcl_AppendPrintfToObj(opcodes, "jump end%ld; label else%ld; ", lab, lab);
    colonPrec = Tcl_ObjGetVar2(interp, st->v_inprec, st->colonObj, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
    if (colonPrec == NULL || (sub = CallParse(st, interp, colonPrec)) == NULL) goto fail;
    Tcl_AppendObjToObj(opcodes, sub);
    Tcl_DecrRefCount(sub);
    Tcl_AppendPrintfToObj(opcodes, "label end%ld; nop; ", lab);
    return opcodes;
fail:
    Tcl_DecrRefCount(opcodes);
    return NULL;
}

static int CalcParseTernary(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 1) { Tcl_WrongNumArgs(interp, 1, objv, NULL); return TCL_ERROR; }
    r = ParseTernary((CalcState *) cd, interp);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * parsePattern
 * A pattern is a name, or a parenthesised comma list of patterns.
 * Returns {n name} for a name, {t child child ...} for a tuple, leaves tokpos after it.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *ParsePattern(CalcState *st, Tcl_Interp *interp) {
    long pos;
    Tcl_Obj *token;
    if (GetTokpos(st, interp, &pos) != TCL_OK) return NULL;
    token = TokenAt(st, interp, pos);
    if (token == NULL) return NULL;
    Tcl_IncrRefCount(token);
    if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(token); return NULL; }
    if (strcmp(Tcl_GetString(token), "(") == 0) {
        Tcl_Obj *kids = Tcl_NewListObj(0, NULL);
        Tcl_DecrRefCount(token);
        Tcl_IncrRefCount(kids);
        Tcl_ListObjAppendElement(NULL, kids, S("t"));
        for (;;) {
            Tcl_Obj *kid = ParsePattern(st, interp);
            if (kid == NULL) { Tcl_DecrRefCount(kids); return NULL; }
            Tcl_ListObjAppendElement(NULL, kids, kid);
            Tcl_DecrRefCount(kid);
            if (GetTokpos(st, interp, &pos) != TCL_OK) { Tcl_DecrRefCount(kids); return NULL; }
            token = TokenAt(st, interp, pos);
            if (token == NULL) { Tcl_DecrRefCount(kids); return NULL; }
            Tcl_IncrRefCount(token);
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(token); Tcl_DecrRefCount(kids); return NULL; }
            if (strcmp(Tcl_GetString(token), ")") == 0) { Tcl_DecrRefCount(token); break; }
            if (strcmp(Tcl_GetString(token), ",") != 0) {
                CALC_ERROR(interp, "Calc: expected ',' or ')' in a pattern but found '%s'", Tcl_GetString(token));
                Tcl_DecrRefCount(token); Tcl_DecrRefCount(kids);
                return NULL;
            }
            Tcl_DecrRefCount(token);
        }
        return kids;
    }
    if (!IsName(st, interp, token)) {
        CALC_ERROR(interp, "Calc: expected a variable name in a pattern but found '%s'", Tcl_GetString(token));
        Tcl_DecrRefCount(token);
        return NULL;
    }
    {
        Tcl_Obj *r = Tcl_NewListObj(0, NULL);
        Tcl_IncrRefCount(r);
        Tcl_ListObjAppendElement(NULL, r, S("n"));
        Tcl_ListObjAppendElement(NULL, r, token);
        Tcl_DecrRefCount(token);
        return r;
    }
}

static int CalcParsePattern(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 1) { Tcl_WrongNumArgs(interp, 1, objv, NULL); return TCL_ERROR; }
    r = ParsePattern((CalcState *) cd, interp);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * parsePrefix token
 * Parses a primary: number, boolean, parenthesised expression or tuple pattern, unary
 * operator, variable, assignment, increment, function call or array reference.
 * Returns the TAL (new object, refcount 1) or NULL on error.
 * ---------------------------------------------------------------------------------- */

/* [namespace which name] resolved from ::Calc, the namespace the Tcl proc lived in */
static Tcl_Obj *WhichCmd(Tcl_Interp *interp, Tcl_Obj *name) {
    Tcl_Namespace *ns = Tcl_FindNamespace(interp, "::Calc", NULL, 0);
    Tcl_Command cmd = Tcl_FindCommand(interp, Tcl_GetString(name), ns, 0);
    Tcl_Obj *r;
    if (cmd == NULL) return NULL;
    r = Tcl_NewObj();
    Tcl_IncrRefCount(r);
    Tcl_GetCommandFullName(interp, cmd, r);
    return r;
}

/* [string is digit -strict [string index $token 0]] */
static int FirstIsDigit(Tcl_Interp *interp, Tcl_Obj *token, int *res) {
    const char *p = Tcl_GetString(token);
    unsigned char c = (unsigned char) p[0];
    if (c == 0) { *res = 0; return TCL_OK; }
    if (c < 0x80) { *res = (c >= '0' && c <= '9'); return TCL_OK; }
    {
        Tcl_Obj *first = Tcl_NewStringObj(p, (Tcl_Size) (Tcl_UtfNext(p) - p));
        int rc;
        Tcl_IncrRefCount(first);
        rc = StrIs(interp, "digit", 1, first, res);
        Tcl_DecrRefCount(first);
        return rc;
    }
}

/* [string is entier $t] || [string is double $t], with fast answers for the common cases */
static int LooksNumeric(Tcl_Interp *interp, Tcl_Obj *token, int *res) {
    Tcl_Size len, i;
    const char *p = Tcl_GetStringFromObj(token, &len);
    int alldigits = 1, rc;
    *res = 0;
    if (len == 0) {
        /* without -strict the empty string is a number: this is how "1 +" gets "push ; " */
        rc = StrIs(interp, "entier", 0, token, res);
        if (rc == TCL_OK && !*res) rc = StrIs(interp, "double", 0, token, res);
        return rc;
    }
    for (i = 0; i < len; i++) if (p[i] < '0' || p[i] > '9') { alldigits = 0; break; }
    if (alldigits && len <= 18) { *res = 1; return TCL_OK; }
    if (len == 1) { *res = 0; return TCL_OK; }            /* a lone character that is not a digit */
    {
        char c = p[0];
        if (!((c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.' || c == 'i' || c == 'I' || c == 'n' || c == 'N')) {
            /* a number cannot start with this character (whitespace is trimmed already) */
            if (!((unsigned char) c >= 0x80 || c == ' ' || (c >= 0x09 && c <= 0x0D))) return TCL_OK;
        }
    }
    rc = StrIs(interp, "entier", 0, token, res);
    if (rc == TCL_OK && !*res) rc = StrIs(interp, "double", 0, token, res);
    return rc;
}

/* regsub -all {_} $token {} token; regsub {^0[dD]} $token {} token */
static Tcl_Obj *StripNumber(Tcl_Obj *token) {
    Tcl_Size len, i, n = 0;
    const char *p = Tcl_GetStringFromObj(token, &len);
    char *buf = Tcl_Alloc((size_t) len + 1);
    Tcl_Obj *r;
    const char *q = buf;
    for (i = 0; i < len; i++) if (p[i] != '_') buf[n++] = p[i];
    buf[n] = '\0';
    if (n >= 2 && q[0] == '0' && (q[1] == 'd' || q[1] == 'D')) { q += 2; n -= 2; }
    r = Tcl_NewStringObj(q, n);
    Tcl_IncrRefCount(r);
    Tcl_Free(buf);
    return r;
}

static Tcl_Obj *ParsePrefix(CalcState *st, Tcl_Interp *interp, Tcl_Obj *tokenIn) {
    Tcl_Obj *token = tokenIn, *res = NULL, *nexttok, *sub;
    long pos;
    const char *ts;
    int isnum;
    Tcl_IncrRefCount(token);
    ts = Tcl_GetString(token);

    /* ++name / --name : prefix increment/decrement, the value is the new value */
    if (strcmp(ts, "++") == 0 || strcmp(ts, "--") == 0) {
        Tcl_Obj *vname;
        if (GetTokpos(st, interp, &pos) != TCL_OK || (vname = TokenAt(st, interp, pos)) == NULL) goto done;
        if (!IsName(st, interp, vname)) {
            CALC_ERROR(interp, "Calc: expected a variable name after '%s' but found '%s'", ts, Tcl_GetString(vname));
            goto done;
        }
        if (SetTokpos(st, interp, pos + 1) != TCL_OK) goto done;
        res = IncrCode(st, interp, Tcl_GetString(vname), ts[0] == '+' ? "1" : "-1");
        goto done;
    }

    /* a number?  Underscores and a 0d prefix are removed, even if it then is no number */
    if (FirstIsDigit(interp, token, &isnum) != TCL_OK) goto done;
    if (isnum) {
        Tcl_Obj *nt = StripNumber(token);
        Tcl_DecrRefCount(token);
        token = nt;
        ts = Tcl_GetString(token);
    }
    if (LooksNumeric(interp, token, &isnum) != TCL_OK) goto done;
    if (isnum) {
        if (GetTokpos(st, interp, &pos) != TCL_OK || (nexttok = TokenAt(st, interp, pos)) == NULL) goto done;
        if (strcmp(Tcl_GetString(nexttok), "=") == 0) {
            CALC_ERROR(interp, "Calc: cannot assign to number literal '%s'", ts);
            goto done;
        }
        res = Tcl_ObjPrintf("push %s; ", ts);
        Tcl_IncrRefCount(res);
        goto done;
    }

    /* a boolean? */
    if (strcmp(ts, "false") == 0 || strcmp(ts, "true") == 0) {
        if (GetTokpos(st, interp, &pos) != TCL_OK || (nexttok = TokenAt(st, interp, pos)) == NULL) goto done;
        if (strcmp(Tcl_GetString(nexttok), "=") == 0) {
            CALC_ERROR(interp, "Calc: cannot assign to boolean literal '%s'", ts);
            goto done;
        }
        res = Tcl_ObjPrintf("push %s; ", ts);
        Tcl_IncrRefCount(res);
        goto done;
    }

    /* a parenthesised subexpression, a list (e1, e2, ...) or a tuple pattern (a,b) = expr */
    if (strcmp(ts, "(") == 0) {
        Tcl_Obj *toks, **el, *opcodes;
        Tcl_Size n, j;
        int d = 1;
        long pos0;
        if (GetTokpos(st, interp, &pos0) != TCL_OK) goto done;
        if ((toks = GetVar(interp, st->v_tokens)) == NULL || Tcl_ListObjGetElements(interp, toks, &n, &el) != TCL_OK) goto done;
        for (j = (Tcl_Size) pos0; j < n; j++) {
            const char *tj = Tcl_GetString(el[j]);
            if (strcmp(tj, "(") == 0) d++;
            else if (strcmp(tj, ")") == 0) { if (--d == 0) break; }
        }
        if (j < n && j + 1 < n && strcmp(Tcl_GetString(el[j + 1]), "=") == 0) {
            Tcl_Obj *pat, *ds;
            long p2;
            if (SetTokpos(st, interp, pos0 - 1) != TCL_OK) goto done;
            if ((pat = ParsePattern(st, interp)) == NULL) goto done;
            if (GetTokpos(st, interp, &p2) != TCL_OK || SetTokpos(st, interp, p2 + 1) != TCL_OK) { Tcl_DecrRefCount(pat); goto done; }
            if ((sub = CallParse(st, interp, st->zeroObj)) == NULL) { Tcl_DecrRefCount(pat); goto done; }
            ds = Destructure(st, interp, pat);
            Tcl_DecrRefCount(pat);
            if (ds == NULL) { Tcl_DecrRefCount(sub); goto done; }
            Tcl_AppendObjToObj(sub, ds);
            Tcl_DecrRefCount(ds);
            res = sub;
            goto done;
        }
        if ((opcodes = CallParse(st, interp, st->zeroObj)) == NULL) goto done;
        {
            long cnt = 1;
            Tcl_Obj *t;
            if (GetTokpos(st, interp, &pos) != TCL_OK || (t = TokenAt(st, interp, pos)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
            while (strcmp(Tcl_GetString(t), ",") == 0) {
                if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(opcodes); goto done; }
                if ((sub = CallParse(st, interp, st->zeroObj)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
                Tcl_AppendObjToObj(opcodes, sub);
                Tcl_DecrRefCount(sub);
                cnt++;
                if (GetTokpos(st, interp, &pos) != TCL_OK || (t = TokenAt(st, interp, pos)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
            }
            if (strcmp(Tcl_GetString(t), ")") == 0) {
                if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(opcodes); goto done; }
                if (cnt > 1) Tcl_AppendPrintfToObj(opcodes, "list %ld; ", cnt);
                res = opcodes;
                goto done;
            }
            CALC_ERROR(interp, "Calc: expected ')' but found '%s'", Tcl_GetString(t));
            Tcl_DecrRefCount(opcodes);
            goto done;
        }
    }

    /* a unary operator? */
    if (strcmp(ts, "+") == 0 || strcmp(ts, "-") == 0 || strcmp(ts, "!") == 0 || strcmp(ts, "~") == 0) {
        Tcl_Obj *pre, *code;
        pre = GetVar(interp, st->v_preprec);
        if (pre == NULL) goto done;
        if ((sub = CallParse(st, interp, pre)) == NULL) goto done;
        code = Tcl_ObjGetVar2(interp, st->v_precode, token, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
        if (code == NULL) { Tcl_DecrRefCount(sub); goto done; }
        res = sub;
        Tcl_AppendPrintfToObj(res, "%s; ", Tcl_GetString(code));
        goto done;
    }

    /* build a name (it may contain ::) */
    {
        Tcl_Obj *name = Tcl_NewObj(), *nm;
        Tcl_IncrRefCount(name);
        for (;;) {
            Tcl_Obj *nt;
            if (!(strcmp(Tcl_GetString(token), "::") == 0 || IsName(st, interp, token))) break;
            Tcl_AppendObjToObj(name, token);
            if (GetTokpos(st, interp, &pos) != TCL_OK || (nt = TokenAt(st, interp, pos)) == NULL) { Tcl_DecrRefCount(name); goto done; }
            Tcl_DecrRefCount(token);
            token = nt;
            Tcl_IncrRefCount(token);
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(name); goto done; }
        }
        ts = Tcl_GetString(token);
        nm = name;
        if (Tcl_GetCharLength(nm) == 0) {
            Tcl_DecrRefCount(name);
            CALC_ERROR(interp, "Calc: expected start of expression but found '%s'", ts);
            goto done;
        }
        /* a name: look at what follows (it is the token that ended the loop) */
        if (GetTokpos(st, interp, &pos) != TCL_OK || SetTokpos(st, interp, pos - 1) != TCL_OK) { Tcl_DecrRefCount(name); goto done; }
        pos -= 1;
        if (strcmp(ts, "(") == 0) {
            Tcl_Obj *nf, *fun;
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(name); goto done; }
            pos += 1;
            /* native function first */
            nf = Tcl_ObjGetVar2(interp, st->v_nativeFunc, nm, TCL_GLOBAL_ONLY);
            if (nf != NULL) {
                res = ParseFuncArgs(st, interp, nm, nf);
                Tcl_DecrRefCount(name);
                goto done;
            }
            /* a mathfunc second, any command or proc third */
            {
                Tcl_Obj *full = Tcl_ObjPrintf("tcl::mathfunc::%s", Tcl_GetString(nm));
                Tcl_IncrRefCount(full);
                fun = WhichCmd(interp, full);
                Tcl_DecrRefCount(full);
            }
            if (fun == NULL) fun = WhichCmd(interp, nm);
            if (fun != NULL) {
                Tcl_Obj *args = ParseFuncArgs(st, interp, nm, st->emptyObj);
                if (args != NULL) {
                    res = Tcl_ObjPrintf("push %s; ", Tcl_GetString(fun));
                    Tcl_IncrRefCount(res);
                    Tcl_AppendObjToObj(res, args);
                    Tcl_DecrRefCount(args);
                }
                Tcl_DecrRefCount(fun);
                Tcl_DecrRefCount(name);
                goto done;
            }
            /* an array reference */
            {
                Tcl_Obj *opcodes = Tcl_ObjPrintf("push %s; ", Tcl_GetString(nm)), *t;
                long indexCount = 1;
                Tcl_IncrRefCount(opcodes);
                Tcl_DecrRefCount(name);
                if ((sub = CallParse(st, interp, st->zeroObj)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
                Tcl_AppendObjToObj(opcodes, sub);
                Tcl_DecrRefCount(sub);
                if (GetTokpos(st, interp, &pos) != TCL_OK || (t = TokenAt(st, interp, pos)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
                while (strcmp(Tcl_GetString(t), ",") == 0) {
                    if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(opcodes); goto done; }
                    indexCount++;
                    Tcl_AppendToObj(opcodes, "push {,}; ", -1);
                    if ((sub = CallParse(st, interp, st->zeroObj)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
                    Tcl_AppendObjToObj(opcodes, sub);
                    Tcl_DecrRefCount(sub);
                    if (GetTokpos(st, interp, &pos) != TCL_OK || (t = TokenAt(st, interp, pos)) == NULL) { Tcl_DecrRefCount(opcodes); goto done; }
                }
                if (strcmp(Tcl_GetString(t), ")") != 0) {
                    CALC_ERROR(interp, "Calc: expected ')' but found '%s'", Tcl_GetString(t));
                    Tcl_DecrRefCount(opcodes);
                    goto done;
                }
                if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(opcodes); goto done; }
                if (indexCount > 1) Tcl_AppendPrintfToObj(opcodes, "strcat %ld; ", indexCount * 2 - 1);
                Tcl_AppendToObj(opcodes, "loadArrayStk; ", -1);
                res = opcodes;
                goto done;
            }
        }
        /* not followed by ( */
        if (strcmp(ts, "=") == 0) {
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(name); goto done; }
            if ((sub = CallParse(st, interp, st->zeroObj)) == NULL) { Tcl_DecrRefCount(name); goto done; }
            res = Tcl_ObjPrintf("push %s; ", Tcl_GetString(nm));
            Tcl_IncrRefCount(res);
            Tcl_AppendObjToObj(res, sub);
            Tcl_AppendToObj(res, "storeStk; ", -1);
            Tcl_DecrRefCount(sub);
            Tcl_DecrRefCount(name);
            goto done;
        }
        if (strcmp(ts, "++") == 0 || strcmp(ts, "--") == 0) {
            const char *delta = ts[0] == '+' ? "1" : "-1";
            Tcl_Obj *ic;
            if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(name); goto done; }
            ic = IncrCode(st, interp, Tcl_GetString(nm), delta);
            if (ic != NULL) {
                res = Tcl_DuplicateObj(ic);
                Tcl_IncrRefCount(res);
                Tcl_DecrRefCount(ic);
                Tcl_AppendPrintfToObj(res, "push %s; sub; ", delta);
            }
            Tcl_DecrRefCount(name);
            goto done;
        }
        res = Tcl_ObjPrintf("push %s; loadStk; ", Tcl_GetString(nm));
        Tcl_IncrRefCount(res);
        Tcl_DecrRefCount(name);
    }
done:
    Tcl_DecrRefCount(token);
    return res;
}

static int CalcParsePrefix(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "token"); return TCL_ERROR; }
    r = ParsePrefix((CalcState *) cd, interp, objv[1]);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * parse min_prec
 * Pratt parser: parses an expression until an operator whose precedence is lower than
 * min_prec.  The tokens are in ::Calc::tokens, the position in ::Calc::tokpos.
 * Returns the TAL (new object, refcount 1) or NULL on error.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *Parse(CalcState *st, Tcl_Interp *interp, double minPrec) {
    long pos;
    Tcl_Obj *token, *opcodes, *sub;

    if (GetTokpos(st, interp, &pos) != TCL_OK || (token = TokenAt(st, interp, pos)) == NULL) return NULL;
    Tcl_IncrRefCount(token);
    if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(token); return NULL; }
    opcodes = ParsePrefix(st, interp, token);
    Tcl_DecrRefCount(token);
    if (opcodes == NULL) return NULL;

    for (;;) {
        Tcl_Obj *toks, *pv, *code;
        Tcl_Size n;
        const char *ts;
        double tokPrec;

        if ((toks = GetVar(interp, st->v_tokens)) == NULL || Tcl_ListObjLength(interp, toks, &n) != TCL_OK) goto fail;
        if (GetTokpos(st, interp, &pos) != TCL_OK) goto fail;
        if (pos >= (long) n) break;
        if ((token = TokenAt(st, interp, pos)) == NULL) goto fail;
        Tcl_IncrRefCount(token);
        ts = Tcl_GetString(token);

        pv = Tcl_ObjGetVar2(interp, st->v_inprec, token, TCL_GLOBAL_ONLY);
        if (pv == NULL) {
            CALC_ERROR(interp, "Calc: expected operator but found '%s'", ts);
            Tcl_DecrRefCount(token);
            goto fail;
        }
        if (Tcl_GetDoubleFromObj(interp, pv, &tokPrec) != TCL_OK) { Tcl_DecrRefCount(token); goto fail; }
        if (tokPrec < minPrec) { Tcl_DecrRefCount(token); break; }
        /* binary operators are left associative, except ** and the ternary ? */
        if (tokPrec == minPrec && strcmp(ts, "**") != 0 && strcmp(ts, "?") != 0) { Tcl_DecrRefCount(token); break; }

        if (SetTokpos(st, interp, pos + 1) != TCL_OK) { Tcl_DecrRefCount(token); goto fail; }

        if (strcmp(ts, "?") == 0) {
            Tcl_DecrRefCount(token);
            if ((sub = ParseTernary(st, interp)) == NULL) goto fail;
            Tcl_AppendObjToObj(opcodes, sub);
            Tcl_DecrRefCount(sub);
            continue;
        }
        if (strcmp(ts, "->") == 0 || strcmp(ts, "=>") == 0) {
            /* value -> pattern ; value => pattern is strict: every group must have exactly the
               number of elements of the pattern.  The strict flag is reset only after success. */
            int strict = (ts[0] == '=');
            Tcl_Obj *pat, *ds;
            Tcl_DecrRefCount(token);
            if (strict && SetVarInt(interp, st->v_strictFlag, 1) != TCL_OK) goto fail;
            if ((pat = ParsePattern(st, interp)) == NULL) goto fail;
            ds = Destructure(st, interp, pat);
            Tcl_DecrRefCount(pat);
            if (ds == NULL) goto fail;
            Tcl_AppendObjToObj(opcodes, ds);
            Tcl_DecrRefCount(ds);
            if (strict && SetVarInt(interp, st->v_strictFlag, 0) != TCL_OK) goto fail;
            continue;
        }
        if (strcmp(ts, "||") == 0 || strcmp(ts, "&&") == 0) {
            /* short circuit: the jump comes straight after the left operand, so the code of the
               right operand is skipped when the left one decides, as in expr.
                 a || b :  <a> jumpTrue T  <b> jumpTrue T  push 0  jump E  label T  push 1  label E
                 a && b :  <a> jumpFalse F <b> jumpFalse F push 1  jump E  label F  push 0  label E
               The labels are made unique per use so several can appear in one assemble. */
            long lab;
            const char *jmp = ts[0] == '|' ? "jumpTrue" : "jumpFalse";
            int fall = ts[0] == '|' ? 0 : 1, hit = 1 - fall;
            Tcl_DecrRefCount(token);
            if (NextLabel(st, interp, &lab) != TCL_OK) goto fail;
            Tcl_AppendPrintfToObj(opcodes, "%s sc_hit%ld; ", jmp, lab);
            if ((sub = Parse(st, interp, tokPrec)) == NULL) goto fail;
            Tcl_AppendObjToObj(opcodes, sub);
            Tcl_DecrRefCount(sub);
            Tcl_AppendPrintfToObj(opcodes, "%s sc_hit%ld; push %d; jump sc_end%ld; label sc_hit%ld; push %d; label sc_end%ld; ",
                                  jmp, lab, fall, lab, lab, hit, lab);
            continue;
        }
        /* an ordinary infix operator */
        code = Tcl_ObjGetVar2(interp, st->v_incode, token, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
        Tcl_DecrRefCount(token);
        if (code == NULL) goto fail;
        Tcl_IncrRefCount(code);
        if ((sub = Parse(st, interp, tokPrec)) == NULL) { Tcl_DecrRefCount(code); goto fail; }
        Tcl_AppendObjToObj(opcodes, sub);
        Tcl_DecrRefCount(sub);
        Tcl_AppendPrintfToObj(opcodes, "%s; ", Tcl_GetString(code));
        Tcl_DecrRefCount(code);
    }
    return opcodes;
fail:
    Tcl_DecrRefCount(opcodes);
    return NULL;
}

static int CalcParse(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    double p;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "min_prec"); return TCL_ERROR; }
    if (Tcl_GetDoubleFromObj(interp, objv[1], &p) != TCL_OK) return TCL_ERROR;
    r = Parse((CalcState *) cd, interp, p);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * compile toks
 * Compiles a token list to TAL.
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *Compile(CalcState *st, Tcl_Interp *interp, Tcl_Obj *toks) {
    Tcl_Size n;
    if (Tcl_ListObjLength(interp, toks, &n) != TCL_OK) return NULL;
    if (n == 0) { CALC_ERROR(interp, "Calc: nothing to calculate"); return NULL; }
    if (Tcl_ObjSetVar2(interp, st->v_tokens, NULL, toks, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG) == NULL) return NULL;
    if (SetTokpos(st, interp, 0) != TCL_OK) return NULL;
    return Parse(st, interp, 0.0);
}

static int CalcCompile(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *r;
    if (objc != 2) { Tcl_WrongNumArgs(interp, 1, objv, "toks"); return TCL_ERROR; }
    r = Compile((CalcState *) cd, interp, objv[1]);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * compile0 exp ?inproc?
 * Splits the text into statements and compiles each one.
 *   - # to the end of the line is a comment, ' is a statement separator like ;
 *   - newlines are blanks, $ is dropped, empty statements are ignored
 *   - inside a proc, "push name; <expr>; storeStk;" becomes "<expr>; store name;"
 * ---------------------------------------------------------------------------------- */
static Tcl_Obj *PeepSub(Tcl_Obj *obj, Tcl_Size start, Tcl_Size endExcl) {
    Tcl_Obj *r = (endExcl <= start) ? Tcl_NewObj() : Tcl_GetRange(obj, start, endExcl - 1);
    Tcl_IncrRefCount(r);
    return r;
}

static Tcl_Obj *Peephole(CalcState *st, Tcl_Interp *interp, Tcl_Obj *code) {
    /* regexp {^push ([[:alpha:]_][^:;\s]*); (.*); storeStk; $} $code -> name expr */
    Tcl_RegExp re = Tcl_GetRegExpFromObj(interp, st->peepRe, TCL_REG_ADVANCED);
    Tcl_RegExpInfo info;
    int rc;
    if (re == NULL) return NULL;
    rc = Tcl_RegExpExecObj(interp, re, code, 0, 3, 0);
    if (rc < 0) return NULL;
    if (rc == 0) { Tcl_IncrRefCount(code); return code; }
    Tcl_RegExpGetInfo(re, &info);
    {
        Tcl_Obj *name = PeepSub(code, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
        Tcl_Obj *expr = PeepSub(code, (Tcl_Size) info.matches[2].start, (Tcl_Size) info.matches[2].end);
        Tcl_Obj *r;
        r = Tcl_ObjPrintf("%s; store %s; ", Tcl_GetString(expr), Tcl_GetString(name));
        Tcl_IncrRefCount(r);
        Tcl_DecrRefCount(name);
        Tcl_DecrRefCount(expr);
        return r;
    }
}

/* compile one statement; the tokens must all be consumed.  what is "expression" or "statement" */
static Tcl_Obj *CompileStatement(CalcState *st, Tcl_Interp *interp, Tcl_Obj *text, const char *what, int inproc) {
    Tcl_Obj *toks, *code, *toksVar, *r;
    long pos;
    Tcl_Size n;
    if (Tokenise(interp, text, &toks) != TCL_OK) return NULL;
    code = Compile(st, interp, toks);
    Tcl_DecrRefCount(toks);
    if (code == NULL) return NULL;
    if (GetTokpos(st, interp, &pos) != TCL_OK || (toksVar = GetVar(interp, st->v_tokens)) == NULL
        || Tcl_ListObjLength(interp, toksVar, &n) != TCL_OK) { Tcl_DecrRefCount(code); return NULL; }
    if (pos < (long) n) {
        Tcl_Obj *t = TokenAt(st, interp, pos);
        CALC_ERROR(interp, "Calc: unexpected token '%s' in %s '%s'", t ? Tcl_GetString(t) : "", what, Tcl_GetString(text));
        Tcl_DecrRefCount(code);
        return NULL;
    }
    if (inproc) {
        r = Peephole(st, interp, code);
        Tcl_DecrRefCount(code);
        return r;
    }
    return code;
}

static Tcl_Obj *Compile0(CalcState *st, Tcl_Interp *interp, Tcl_Obj *expObj, Tcl_Obj *inprocObj) {
    int inproc;
    Tcl_Size len, i;
    const char *src;
    char *buf;
    Tcl_Obj *exp, *result = NULL, *sz;
    long cacheSize;

    /* set inprocFlag $inproc */
    if (Tcl_ObjSetVar2(interp, st->v_inprocFlag, NULL, inprocObj, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG) == NULL) return NULL;
    if (RunV(interp, &sz, 3, S("array"), S("size"), S("::Calc::cache")) != TCL_OK) return NULL;
    if (Tcl_GetLongFromObj(interp, sz, &cacheSize) != TCL_OK) { Tcl_DecrRefCount(sz); return NULL; }
    Tcl_DecrRefCount(sz);
    if (cacheSize > 1000) {
        /* a programmer error: a $variable inside a Calc expression makes every call a new key */
        CALC_ERROR(interp, "Calc: Cache exhausted with expression: %s", Tcl_GetString(expObj));
        return NULL;
    }

    /* remove comments (# to the end of the line, the newline stays); ' is ; , newline is blank, $ is dropped */
    src = Tcl_GetStringFromObj(expObj, &len);
    buf = Tcl_Alloc((size_t) len + 1);
    {
        Tcl_Size n = 0;
        char *tmp = Tcl_Alloc((size_t) len + 1);
        Tcl_Size m = 0;
        for (i = 0; i < len; i++) {
            if (src[i] == '#') { while (i + 1 < len && src[i + 1] != '\n') i++; continue; }
            tmp[m++] = src[i];
        }
        for (i = 0; i < m; i++) {
            char c = tmp[i];
            if (c == '\'') buf[n++] = ';';
            else if (c == '\n') buf[n++] = ' ';
            else if (c == '$') continue;
            else buf[n++] = c;
        }
        buf[n] = '\0';
        len = n;
        Tcl_Free(tmp);
    }
    exp = Tcl_NewStringObj(buf, len);
    Tcl_IncrRefCount(exp);
    Tcl_Free(buf);

    if (Tcl_GetBooleanFromObj(interp, inprocObj, &inproc) != TCL_OK) { Tcl_DecrRefCount(exp); return NULL; }

    if (memchr(Tcl_GetString(exp), ';', (size_t) len) == NULL) {
        /* fast path: a single statement */
        result = CompileStatement(st, interp, exp, "expression", inproc);
        Tcl_DecrRefCount(exp);
        return result;
    }

    /* several statements */
    {
        const char *p = Tcl_GetString(exp), *end = p + len, *seg = p;
        int count = 0;
        result = Tcl_NewObj();
        Tcl_IncrRefCount(result);
        for (;;) {
            const char *semi = (const char *) memchr(seg, ';', (size_t) (end - seg));
            const char *se = semi ? semi : end, *ss = seg;
            TrimRange(&ss, &se);
            if (se > ss) {
                Tcl_Obj *stmt = Tcl_NewStringObj(ss, (Tcl_Size) (se - ss)), *code;
                Tcl_IncrRefCount(stmt);
                if (count > 0) Tcl_AppendToObj(result, "pop; ", -1);
                code = CompileStatement(st, interp, stmt, "statement", inproc);
                Tcl_DecrRefCount(stmt);
                if (code == NULL) { Tcl_DecrRefCount(result); Tcl_DecrRefCount(exp); return NULL; }
                Tcl_AppendObjToObj(result, code);
                Tcl_DecrRefCount(code);
                count++;
            }
            if (semi == NULL) break;
            seg = semi + 1;
        }
    }
    Tcl_DecrRefCount(exp);
    return result;
}

static int CalcCompile0(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    CalcState *st = (CalcState *) cd;
    Tcl_Obj *r;
    if (objc < 2 || objc > 3) { Tcl_WrongNumArgs(interp, 1, objv, "exp ?inproc?"); return TCL_ERROR; }
    r = Compile0(st, interp, objv[1], objc == 3 ? objv[2] : st->zeroObj);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * transform= arglist body ?inproc? ?preserve?
 * Rewrites the lines of a proc body that start a Calc expression ("= expr", ": expr",
 * "= { ... }", "[: expr]", "[: { ... }]") into inline  tcl::unsupported::assemble  calls.
 * With preserve the original source is kept in an  if {0} {...}  around the assemble.
 * ---------------------------------------------------------------------------------- */

/* match positions, as character indices from the start of the string (end is exclusive) */
typedef struct { struct { long start, end; } matches[4]; } ReMatch;

/* run a regexp on str starting at char offset; 1 = match (info filled), 0 = none, -1 = error.
   Tcl reports the positions relative to the offset, they are made absolute here. */
static int ReExec(Tcl_Interp *interp, Tcl_Obj *pat, Tcl_Obj *str, Tcl_Size offset, ReMatch *out) {
    Tcl_RegExp re = Tcl_GetRegExpFromObj(interp, pat, TCL_REG_ADVANCED);
    Tcl_RegExpInfo info;
    int rc, k;
    if (re == NULL) return -1;
    rc = Tcl_RegExpExecObj(interp, re, str, offset, -1, 0);
    if (rc > 0) {
        Tcl_RegExpGetInfo(re, &info);
        for (k = 0; k < 4; k++) {
            if (k <= (int) info.nsubs) {
                out->matches[k].start = (long) info.matches[k].start + (info.matches[k].start >= 0 ? (long) offset : 0);
                out->matches[k].end = (long) info.matches[k].end + (info.matches[k].end >= 0 ? (long) offset : 0);
            } else {
                out->matches[k].start = out->matches[k].end = -1;
            }
        }
    }
    return rc;
}

/* the characters [start, endExclusive) of obj as a new object (refcount 1) */
static Tcl_Obj *Sub(Tcl_Obj *obj, Tcl_Size start, Tcl_Size endExcl) {
    /* (Tcl_GetRange treats a last index of -1 as "to the end", so empty ranges are made here) */
    Tcl_Obj *r = (endExcl <= start) ? Tcl_NewObj() : Tcl_GetRange(obj, start, endExcl - 1);
    Tcl_IncrRefCount(r);
    return r;
}

/* regsub {\s*;#[^\n]*$} expr {} expr : drops a trailing ;# comment.  Returns a new object. */
static Tcl_Obj *DropTrailingComment(CalcState *st, Tcl_Interp *interp, Tcl_Obj *expr) {
    ReMatch info;
    int rc = ReExec(interp, st->reComment, expr, 0, &info);
    if (rc < 0) return NULL;
    if (rc == 0) { Tcl_IncrRefCount(expr); return expr; }
    return Sub(expr, 0, (Tcl_Size) info.matches[0].start);
}

/* compile an expression found at the given line; inside a proc  push x; loadStk  becomes  load x */
static Tcl_Obj *TransCompile(CalcState *st, Tcl_Interp *interp, Tcl_Obj *expr, Tcl_Obj *inprocObj, int inproc, long lineNo) {
    Tcl_Obj *tal = Compile0(st, interp, expr, inprocObj);
    if (tal == NULL) {
        CALC_ERROR(interp, "line %ld: %s", lineNo, Tcl_GetString(Tcl_GetObjResult(interp)));
        return NULL;
    }
    if (inproc) {
        /* regsub -all {push ([[:alpha:]_][^:;\s]*); loadStk} $tal {load \1} tal */
        Tcl_Obj *out = Tcl_NewObj(), *piece;
        Tcl_Size off = 0, len = Tcl_GetCharLength(tal);
        ReMatch info;
        int rc;
        Tcl_IncrRefCount(out);
        while ((rc = ReExec(interp, st->reLoad, tal, off, &info)) > 0) {
            Tcl_Size ms = (Tcl_Size) info.matches[0].start, me = (Tcl_Size) info.matches[0].end;
            piece = Sub(tal, off, ms);
            Tcl_AppendObjToObj(out, piece);
            Tcl_DecrRefCount(piece);
            piece = Sub(tal, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
            Tcl_AppendToObj(out, "load ", -1);
            Tcl_AppendObjToObj(out, piece);
            Tcl_DecrRefCount(piece);
            off = me;
            if (me == ms) break;
        }
        if (rc < 0) { Tcl_DecrRefCount(out); Tcl_DecrRefCount(tal); return NULL; }
        piece = Sub(tal, off, len);
        Tcl_AppendObjToObj(out, piece);
        Tcl_DecrRefCount(piece);
        Tcl_DecrRefCount(tal);
        tal = out;
    }
    return tal;
}

static Tcl_Obj *Transform(CalcState *st, Tcl_Interp *interp, Tcl_Obj *body, Tcl_Obj *inprocObj, Tcl_Obj *preserveObj) {
    Tcl_Obj *result, *lines, **lv;
    Tcl_Size nLines, i;
    int inproc, preserve;
    ReMatch info;

    if (Tcl_GetBooleanFromObj(NULL, inprocObj, &inproc) != TCL_OK) inproc = -1;   /* the error shows where the Tcl code used it */
    if (Tcl_GetBooleanFromObj(NULL, preserveObj, &preserve) != TCL_OK) preserve = -1;

    /* lines = [split $body \n] */
    lines = Tcl_NewListObj(0, NULL);
    Tcl_IncrRefCount(lines);
    {
        Tcl_Size blen, from = 0, k;
        const char *b = Tcl_GetStringFromObj(body, &blen);
        for (k = 0; blen > 0 && k <= blen; k++) {          /* split of the empty string is the empty list */
            if (k == blen || b[k] == '\n') {
                Tcl_ListObjAppendElement(NULL, lines, Tcl_NewStringObj(b + from, k - from));
                from = k + 1;
            }
        }
    }
    Tcl_ListObjGetElements(NULL, lines, &nLines, &lv);
    result = Tcl_NewObj();
    Tcl_IncrRefCount(result);

#define LINE_AT(ix) (((ix) < nLines) ? lv[(ix)] : st->emptyObj)
#define FAIL() do { Tcl_DecrRefCount(lines); Tcl_DecrRefCount(result); return NULL; } while (0)
#define NEED_BOOL(flag, obj) do { if ((flag) < 0) { int dummy; Tcl_GetBooleanFromObj(interp, (obj), &dummy); FAIL(); } } while (0)

    for (i = 0; i < nLines; i++) {
        Tcl_Obj *line = lv[i];
        int rc;

        /* path 1:  = or : followed by { ... } , possibly over several lines */
        if ((rc = ReExec(interp, st->reP1, line, 0, &info)) < 0) FAIL();
        if (rc > 0) {
            Tcl_Obj *indent = Sub(line, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
            Tcl_Obj *accum = Tcl_DuplicateObj(line), *braced, *expr, *expr2, *tal, *piece, *restObj;
            Tcl_Size startLine = i;
            Tcl_IncrRefCount(accum);
            Tcl_AppendObjToObj(result, indent);
            Tcl_DecrRefCount(indent);
            while (!Tcl_CommandComplete(Tcl_GetString(accum))) {
                i++;
                if (i >= nLines) {
                    CALC_ERROR(interp, "line %ld: Calc: unterminated block", (long) startLine + 1);
                    Tcl_DecrRefCount(accum);
                    FAIL();
                }
                Tcl_AppendToObj(accum, "\n", 1);
                Tcl_AppendObjToObj(accum, lv[i]);
            }
            if ((rc = ReExec(interp, st->reP1b, accum, 0, &info)) < 0) { Tcl_DecrRefCount(accum); FAIL(); }
            if (rc == 0) {
                CALC_ERROR(interp, "can't read \"braced\": no such variable");
                Tcl_DecrRefCount(accum);
                FAIL();
            }
            /* the block ends at the first closing brace that makes it a complete word; code after it
             * on that line (such as  ; return $x ) is kept */
            {
                Tcl_Size open = (Tcl_Size) info.matches[1].start, alen = Tcl_GetCharLength(accum), p = open, endB = (Tcl_Size) info.matches[1].end;
                int found = 0;
                while (++p < alen) {
                    if (Tcl_GetUniChar(accum, p) != '}') continue;
                    piece = Sub(accum, open, p + 1);
                    found = Tcl_CommandComplete(Tcl_GetString(piece));
                    Tcl_DecrRefCount(piece);
                    if (found) { endB = p + 1; break; }
                }
                braced = Sub(accum, open, endB);
                restObj = NULL;
                if (found) {
                    Tcl_Obj *r = Sub(accum, endB, alen);
                    const char *rs = Tcl_GetString(r), *q = rs;
                    while (*q && strchr(" \t\n\r\f\v", *q)) q++;
                    if (*q == ';') { q++; while (*q && strchr(" \t\n\r\f\v", *q)) q++; }
                    if (*q == '\0' || *q == '#') {
                        Tcl_DecrRefCount(r);
                    } else {
                        q = rs;
                        while (*q && strchr(" \t\n\r\f\v", *q)) q++;
                        restObj = Tcl_NewStringObj(*q == ';' ? "" : "; ", -1);
                        Tcl_AppendToObj(restObj, q, -1);
                        Tcl_DecrRefCount(r);
                    }
                }
                if (restObj) Tcl_IncrRefCount(restObj);
            }
            expr = Sub(braced, 1, Tcl_GetCharLength(braced) - 1);
            Tcl_DecrRefCount(braced);
            expr2 = DropTrailingComment(st, interp, expr);
            Tcl_DecrRefCount(expr);
            if (expr2 == NULL) { if (restObj) Tcl_DecrRefCount(restObj); Tcl_DecrRefCount(accum); FAIL(); }
            NEED_BOOL(inproc, inprocObj);
            tal = TransCompile(st, interp, expr2, inprocObj, inproc, (long) i + 1);
            Tcl_DecrRefCount(expr2);
            if (tal == NULL) { if (restObj) Tcl_DecrRefCount(restObj); Tcl_DecrRefCount(accum); FAIL(); }
            NEED_BOOL(preserve, preserveObj);
            if (preserve) {
                Tcl_AppendPrintfToObj(result, "if {0} { %s } { tcl::unsupported::assemble {%s}%s }\n", Tcl_GetString(accum), Tcl_GetString(tal), restObj ? Tcl_GetString(restObj) : "");
            } else {
                Tcl_AppendPrintfToObj(result, "tcl::unsupported::assemble {%s}%s\n", Tcl_GetString(tal), restObj ? Tcl_GetString(restObj) : "");
            }
            if (restObj) Tcl_DecrRefCount(restObj);
            Tcl_DecrRefCount(tal);
            Tcl_DecrRefCount(accum);
            continue;
        }

        /* path 2:  = expr  or  : expr  on one line, no braces */
        if ((rc = ReExec(interp, st->reP2, line, 0, &info)) < 0) FAIL();
        if (rc > 0) {
            Tcl_Obj *indent = Sub(line, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
            Tcl_Obj *expr = Sub(line, (Tcl_Size) info.matches[2].start, (Tcl_Size) info.matches[2].end), *expr2, *tal;
            Tcl_AppendObjToObj(result, indent);
            Tcl_DecrRefCount(indent);
            expr2 = DropTrailingComment(st, interp, expr);
            Tcl_DecrRefCount(expr);
            if (expr2 == NULL) FAIL();
            NEED_BOOL(inproc, inprocObj);
            tal = TransCompile(st, interp, expr2, inprocObj, inproc, (long) i + 1);
            Tcl_DecrRefCount(expr2);
            if (tal == NULL) FAIL();
            NEED_BOOL(preserve, preserveObj);
            if (preserve) {
                Tcl_AppendPrintfToObj(result, "if {0} { %s } { tcl::unsupported::assemble {%s} }\n", Tcl_GetString(line), Tcl_GetString(tal));
            } else {
                Tcl_AppendPrintfToObj(result, "tcl::unsupported::assemble {%s}\n", Tcl_GetString(tal));
            }
            Tcl_DecrRefCount(tal);
            continue;
        }

        /* path 3a:  [: { ... }]  inline, possibly over several lines */
        if ((rc = ReExec(interp, st->reP3a, line, 0, &info)) < 0) FAIL();
        if (rc > 0) {
            Tcl_Size bracketStart = (Tcl_Size) info.matches[0].start;
            Tcl_Obj *prefix = Sub(line, 0, bracketStart);
            Tcl_Obj *accum = Sub(line, bracketStart, Tcl_GetCharLength(line)), *braced, *expr, *expr2, *tal, *suffix;
            Tcl_Size startLine = i;
            while (!Tcl_CommandComplete(Tcl_GetString(accum))) {
                i++;
                if (i >= nLines) {
                    CALC_ERROR(interp, "line %ld: Calc: unterminated block", (long) startLine + 1);
                    Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum);
                    FAIL();
                }
                Tcl_AppendToObj(accum, "\n", 1);
                Tcl_AppendObjToObj(accum, lv[i]);
            }
            if ((rc = ReExec(interp, st->reP3ab, accum, 0, &info)) < 0) { Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum); FAIL(); }
            if (rc == 0) {
                CALC_ERROR(interp, "can't read \"braced\": no such variable");
                Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum);
                FAIL();
            }
            braced = Sub(accum, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
            expr = Sub(braced, 1, Tcl_GetCharLength(braced) - 1);
            Tcl_DecrRefCount(braced);
            expr2 = DropTrailingComment(st, interp, expr);
            Tcl_DecrRefCount(expr);
            if (expr2 == NULL) { Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum); FAIL(); }
            NEED_BOOL(inproc, inprocObj);
            tal = TransCompile(st, interp, expr2, inprocObj, inproc, (long) i + 1);
            Tcl_DecrRefCount(expr2);
            if (tal == NULL) { Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum); FAIL(); }

            /* the text after the closing bracket */
            if ((rc = ReExec(interp, st->reSuffix, accum, 0, &info)) < 0) {
                Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum); Tcl_DecrRefCount(tal); FAIL();
            }
            if (rc > 0) {
                suffix = Sub(accum, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
            } else {
                /* the closing bracket may be on a later line */
                suffix = Tcl_NewObj();
                Tcl_IncrRefCount(suffix);
                while (Tcl_GetCharLength(suffix) == 0 && i < nLines) {
                    const char *p, *e;
                    i++;
                    p = Tcl_GetString(LINE_AT(i));
                    e = p + strlen(p);
                    {
                        int n;
                        while (p < e && (n = TrimAt(p, e)) > 0) p += n;
                    }
                    Tcl_DecrRefCount(suffix);
                    suffix = Tcl_NewStringObj(p, (Tcl_Size) (e - p));
                    Tcl_IncrRefCount(suffix);
                }
            }
            {
                /* strip the leading ] */
                Tcl_Obj *suf2 = Tcl_GetCharLength(suffix) > 0 ? Sub(suffix, 1, Tcl_GetCharLength(suffix)) : Sub(suffix, 0, 0);
                Tcl_DecrRefCount(suffix);
                suffix = suf2;
            }
            NEED_BOOL(preserve, preserveObj);
            if (preserve) {
                Tcl_AppendPrintfToObj(result, "%s[if {0} {%s} {tcl::unsupported::assemble {%s}}]%s\n",
                                      Tcl_GetString(prefix), Tcl_GetString(accum), Tcl_GetString(tal), Tcl_GetString(suffix));
            } else {
                Tcl_AppendPrintfToObj(result, "%s[tcl::unsupported::assemble {%s}]%s\n",
                                      Tcl_GetString(prefix), Tcl_GetString(tal), Tcl_GetString(suffix));
            }
            Tcl_DecrRefCount(prefix); Tcl_DecrRefCount(accum); Tcl_DecrRefCount(tal); Tcl_DecrRefCount(suffix);
            continue;
        }

        /* path 3:  [= expr]  or  [: expr]  on one line, any number of them */
        if ((rc = ReExec(interp, st->reP3, line, 0, &info)) < 0) FAIL();
        if (rc > 0) {
            Tcl_Size pos = 0;
            Tcl_Obj *newline = Tcl_NewObj();
            Tcl_IncrRefCount(newline);
            while ((rc = ReExec(interp, st->reP3c, line, pos, &info)) > 0) {
                Tcl_Size ms = (Tcl_Size) info.matches[0].start, me = (Tcl_Size) info.matches[0].end;
                Tcl_Obj *piece, *expr, *tal;
                piece = Sub(line, pos, ms);
                Tcl_AppendObjToObj(newline, piece);
                Tcl_DecrRefCount(piece);
                expr = Sub(line, (Tcl_Size) info.matches[1].start, (Tcl_Size) info.matches[1].end);
                NEED_BOOL(inproc, inprocObj);
                tal = TransCompile(st, interp, expr, inprocObj, inproc, (long) i + 1);
                if (tal == NULL) { Tcl_DecrRefCount(expr); Tcl_DecrRefCount(newline); FAIL(); }
                Tcl_DecrRefCount(expr);
                NEED_BOOL(preserve, preserveObj);
                if (preserve) {
                    Tcl_Obj *src = Sub(line, ms + 1, me - 1);     /* strip the [ and ] */
                    Tcl_AppendPrintfToObj(newline, "[if {0} {%s} {tcl::unsupported::assemble {%s}}]", Tcl_GetString(src), Tcl_GetString(tal));
                    Tcl_DecrRefCount(src);
                } else {
                    Tcl_AppendPrintfToObj(newline, "[tcl::unsupported::assemble {%s}]", Tcl_GetString(tal));
                }
                Tcl_DecrRefCount(tal);
                pos = me;
            }
            if (rc < 0) { Tcl_DecrRefCount(newline); FAIL(); }
            {
                Tcl_Obj *rest = Sub(line, pos, Tcl_GetCharLength(line));
                Tcl_AppendObjToObj(newline, rest);
                Tcl_DecrRefCount(rest);
            }
            Tcl_AppendObjToObj(result, newline);
            Tcl_AppendToObj(result, "\n", 1);
            Tcl_DecrRefCount(newline);
            continue;
        }

        /* anything else is passed through */
        Tcl_AppendObjToObj(result, line);
        Tcl_AppendToObj(result, "\n", 1);
    }
    Tcl_DecrRefCount(lines);
    return result;
#undef LINE_AT
#undef FAIL
#undef NEED_BOOL
}

static int CalcTransform(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    CalcState *st = (CalcState *) cd;
    Tcl_Obj *r;
    if (objc < 3 || objc > 5) { Tcl_WrongNumArgs(interp, 1, objv, "arglist body ?inproc? ?preserve?"); return TCL_ERROR; }
    r = Transform(st, interp, objv[2], objc > 3 ? objv[3] : st->zeroObj, objc > 4 ? objv[4] : st->oneObj);
    if (r == NULL) return TCL_ERROR;
    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * the debug level that proc=, method=, calc= and transformProcs take:
 *   preserve = max(level, 0)   (becomes the preserve argument of transform=)
 *   abs(level) > 1             (also opens the before/after comparison)
 * Both are computed by Tcl's own max and abs, as the expr in the Tcl code did.
 * ---------------------------------------------------------------------------------- */
static int DebugLevels(Tcl_Interp *interp, Tcl_Obj *level, Tcl_Obj **preserve, int *showDiff) {
    Tcl_Obj *p, *a;
    double d;
    if (RunV(interp, &p, 3, S("::tcl::mathfunc::max"), level, S("0")) != TCL_OK) return TCL_ERROR;
    *preserve = p;
    if (RunV(interp, &a, 2, S("::tcl::mathfunc::abs"), level) != TCL_OK) { Tcl_DecrRefCount(p); return TCL_ERROR; }
    if (Tcl_GetDoubleFromObj(interp, a, &d) != TCL_OK) { Tcl_DecrRefCount(p); Tcl_DecrRefCount(a); return TCL_ERROR; }
    Tcl_DecrRefCount(a);
    *showDiff = d > 1;
    return TCL_OK;
}

/* ------------------------------------------------------------------------------------
 * transformProcs ?-preserve value? ?-regex? ?-listonly? pattern ?pattern ...?
 * Rewrites already defined procs so their Calc expressions are assembled once, as proc= does.
 * ---------------------------------------------------------------------------------- */
/* [namespace tail $name] */
static Tcl_Obj *NamespaceTail(Tcl_Obj *name) {
    const char *base = Tcl_GetString(name), *p = base + strlen(base);
    while (--p > base) {
        if (p[0] == ':' && p[-1] == ':') { p++; break; }
    }
    if (p < base) p = base;
    return Tcl_NewStringObj(p, -1);
}

static int CalcTransformProcs(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    CalcState *st = (CalcState *) cd;
    Tcl_Obj *debug = st->oneObj, *preserve = NULL, *matched = NULL, *touched = NULL, *cand = NULL;
    int useRegex = 0, listOnly = 0, showDiff, rc = TCL_ERROR;
    Tcl_Size ai = 1, i, np, nm;
    Tcl_Obj **pats, **mv;

    /* the optional leading flags */
    while (ai < objc) {
        const char *a = Tcl_GetString(objv[ai]);
        if (strcmp(a, "-preserve") == 0) {
            if (objc - ai < 2) { CALC_ERROR(interp, "Calc::transformProcs: -preserve requires a value"); return TCL_ERROR; }
            debug = objv[ai + 1];
            ai += 2;
        } else if (strcmp(a, "-regex") == 0) {
            useRegex = 1; ai++;
        } else if (strcmp(a, "-listonly") == 0) {
            listOnly = 1; ai++;
        } else {
            break;
        }
    }
    np = objc - ai;
    pats = (Tcl_Obj **) (objv + ai);
    if (np <= 0) { CALC_ERROR(interp, "Calc::transformProcs: no proc name pattern(s) given"); return TCL_ERROR; }

    if (DebugLevels(interp, debug, &preserve, &showDiff) != TCL_OK) return TCL_ERROR;

    /* resolve the patterns to fully qualified proc names */
    matched = Tcl_NewListObj(0, NULL);
    Tcl_IncrRefCount(matched);
    if (!useRegex) {
        for (i = 0; i < np; i++) {
            Tcl_Obj *pat = pats[i], *r, **rv;
            Tcl_Size rn, k;
            if (strncmp(Tcl_GetString(pat), "::", 2) != 0) pat = Tcl_ObjPrintf("::%s", Tcl_GetString(pat));
            if (RunV(interp, &r, 3, S("info"), S("procs"), pat) != TCL_OK) goto out;
            Tcl_ListObjGetElements(NULL, r, &rn, &rv);
            for (k = 0; k < rn; k++) {
                if (strncmp(Tcl_GetString(rv[k]), "::", 2) != 0) Tcl_ListObjAppendElement(NULL, matched, Tcl_ObjPrintf("::%s", Tcl_GetString(rv[k])));
                else Tcl_ListObjAppendElement(NULL, matched, rv[k]);
            }
            Tcl_DecrRefCount(r);
        }
    } else {
        Tcl_Obj **cv;
        Tcl_Size cn, k;
        if ((cand = AllProcsRecursive(interp)) == NULL) goto out;
        Tcl_ListObjGetElements(NULL, cand, &cn, &cv);
        for (i = 0; i < np; i++) {
            int hasNs = strstr(Tcl_GetString(pats[i]), "::") != NULL;
            for (k = 0; k < cn; k++) {
                Tcl_Obj *subject = hasNs ? cv[k] : NamespaceTail(cv[k]);
                int m;
                Tcl_IncrRefCount(subject);
                m = Tcl_RegExpMatchObj(interp, subject, pats[i]);
                Tcl_DecrRefCount(subject);
                if (m < 0) goto out;
                if (m > 0) Tcl_ListObjAppendElement(NULL, matched, cv[k]);
            }
        }
    }
    {
        Tcl_Obj *sorted;
        if (RunV(interp, &sorted, 3, S("lsort"), S("-unique"), matched) != TCL_OK) goto out;
        Tcl_DecrRefCount(matched);
        matched = sorted;
    }
    if (listOnly) { Tcl_SetObjResult(interp, matched); rc = TCL_OK; goto out; }

    /* rebuild each proc from its arguments and body, transform the body, redefine the proc */
    touched = Tcl_NewListObj(0, NULL);
    Tcl_IncrRefCount(touched);
    Tcl_ListObjGetElements(NULL, matched, &nm, &mv);
    for (i = 0; i < nm; i++) {
        Tcl_Obj *procName = mv[i], *args, **av, *arglist = Tcl_NewListObj(0, NULL), *body, *newbody, *r;
        Tcl_Size an, k;
        Tcl_IncrRefCount(arglist);
        if (RunV(interp, &args, 3, S("info"), S("args"), procName) != TCL_OK) { Tcl_DecrRefCount(arglist); goto out; }
        Tcl_ListObjGetElements(NULL, args, &an, &av);
        for (k = 0; k < an; k++) {
            Tcl_Obj *dflt;
            if (RunV(interp, &r, 5, S("info"), S("default"), procName, av[k], st->v_scratch) != TCL_OK) {
                Tcl_DecrRefCount(args); Tcl_DecrRefCount(arglist); goto out;
            }
            {
                int has;
                Tcl_GetBooleanFromObj(NULL, r, &has);
                Tcl_DecrRefCount(r);
                if (has) {
                    Tcl_Obj *pair = Tcl_NewListObj(0, NULL);
                    dflt = GetVar(interp, st->v_scratch);
                    Tcl_ListObjAppendElement(NULL, pair, av[k]);
                    Tcl_ListObjAppendElement(NULL, pair, dflt ? dflt : st->emptyObj);
                    Tcl_ListObjAppendElement(NULL, arglist, pair);
                    Tcl_UnsetVar(interp, Tcl_GetString(st->v_scratch), TCL_GLOBAL_ONLY);
                } else {
                    Tcl_ListObjAppendElement(NULL, arglist, av[k]);
                }
            }
        }
        Tcl_DecrRefCount(args);
        if (RunV(interp, &body, 3, S("info"), S("body"), procName) != TCL_OK) { Tcl_DecrRefCount(arglist); goto out; }

        newbody = Transform(st, interp, body, st->oneObj, preserve);
        if (newbody == NULL) {
            CALC_ERROR(interp, "Calc::transformProcs error compiling '%s'  %s", Tcl_GetString(procName), Tcl_GetString(Tcl_GetObjResult(interp)));
            Tcl_DecrRefCount(arglist); Tcl_DecrRefCount(body);
            goto out;
        }
        if (showDiff) {
            Tcl_Obj *braced = Tcl_ObjPrintf("{%s}", Tcl_GetString(arglist));
            int drc = RunV(interp, NULL, 5, S("::Calc::debug"), procName, braced, body, newbody);
            if (drc != TCL_OK) { Tcl_DecrRefCount(arglist); Tcl_DecrRefCount(body); Tcl_DecrRefCount(newbody); goto out; }
        }
        {
            int prc = RunV(interp, NULL, 4, S("proc"), procName, arglist, newbody);
            Tcl_DecrRefCount(arglist); Tcl_DecrRefCount(body); Tcl_DecrRefCount(newbody);
            if (prc != TCL_OK) goto out;
        }
        Tcl_ListObjAppendElement(NULL, touched, procName);
    }
    Tcl_SetObjResult(interp, touched);
    rc = TCL_OK;
out:
    if (preserve) Tcl_DecrRefCount(preserve);
    if (matched) Tcl_DecrRefCount(matched);
    if (touched) Tcl_DecrRefCount(touched);
    if (cand) Tcl_DecrRefCount(cand);
    return rc;
}

/* ------------------------------------------------------------------------------------
 * : arg ?arg ...?     and its alias     = arg ?arg ...?
 * Compiles the expression once (the TAL is kept in ::Calc::cache) and runs it with
 * assemble in the caller's frame.  Several arguments are joined first, as  join "$arg $args"  did.
 * ---------------------------------------------------------------------------------- */
static int RunAssemble(CalcState *st, Tcl_Interp *interp, Tcl_Obj *tal) {
    if (st->assembleProc == NULL) {
        Tcl_CmdInfo info;
        if (!Tcl_GetCommandInfo(interp, "::tcl::unsupported::assemble", &info) || !info.isNativeObjectProc) {
            /* not resolvable: go through the evaluator */
            return RunV(interp, NULL, 2, S("::tcl::unsupported::assemble"), tal);
        }
        st->assembleProc = info.objProc;
        st->assembleCD = info.objClientData;
        LOCAL_NAME(st->assembleName, "::tcl::unsupported::assemble");
    }
    {
        Tcl_Obj *av[2];
        av[0] = st->assembleName;
        av[1] = tal;
        return st->assembleProc(st->assembleCD, interp, 2, av);
    }
}

static int CalcColon(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    CalcState *st = (CalcState *) cd;
    Tcl_Obj *arg = objv[objc > 1 ? 1 : 0], *tal, *joined = NULL;
    int rc;
    if (objc < 2) { Tcl_WrongNumArgs(interp, 1, objv, "arg ?arg ...?"); return TCL_ERROR; }
    if (objc > 2) {
        /* set arg [join "$arg $args"] : the text is parsed as a list and joined with single blanks */
        Tcl_Obj *rest = Tcl_NewListObj(objc - 2, objv + 2), *text, **el, *out;
        Tcl_Size n, i;
        text = Tcl_ObjPrintf("%s %s", Tcl_GetString(arg), Tcl_GetString(rest));
        Tcl_IncrRefCount(text);
        if (Tcl_ListObjGetElements(interp, text, &n, &el) != TCL_OK) { Tcl_DecrRefCount(text); return TCL_ERROR; }
        out = Tcl_NewObj();
        for (i = 0; i < n; i++) {
            if (i > 0) Tcl_AppendToObj(out, " ", 1);
            Tcl_AppendObjToObj(out, el[i]);
        }
        Tcl_DecrRefCount(text);
        joined = out;
        Tcl_IncrRefCount(joined);
        arg = joined;
    }
    tal = Tcl_ObjGetVar2(interp, st->v_cache, arg, TCL_GLOBAL_ONLY);
    if (tal != NULL) {
        Tcl_IncrRefCount(tal);
        rc = RunAssemble(st, interp, tal);
        Tcl_DecrRefCount(tal);
    } else {
        tal = Compile0(st, interp, arg, st->zeroObj);
        if (tal == NULL) {
            rc = TCL_ERROR;
        } else {
            Tcl_ObjSetVar2(interp, st->v_cache, arg, tal, TCL_GLOBAL_ONLY);
            rc = RunAssemble(st, interp, tal);
            Tcl_DecrRefCount(tal);
        }
    }
    if (joined) Tcl_DecrRefCount(joined);
    return rc;
}

/* ------------------------------------------------------------------------------------
 * calc= label body ?debug?        proc= name arglist body ?debug?        oo::define::method= name arglist body ?debug?
 * Each transforms its body and then evaluates / defines the result in the caller's frame.
 * They were Tcl procs, so a return, break or continue coming out of the evaluated code ends
 * the command itself (like a proc body would) and is not passed on to the caller.
 * ---------------------------------------------------------------------------------- */
static int ProcLikeReturn(Tcl_Interp *interp, int rc) {
    if (rc == TCL_RETURN) {
        Tcl_Obj *opts = Tcl_GetReturnOptions(interp, rc), *v, *kLevel = S("-level"), *kCode = S("-code");
        Tcl_Size level = 1;
        int code = TCL_OK;
        Tcl_IncrRefCount(opts); Tcl_IncrRefCount(kLevel); Tcl_IncrRefCount(kCode);
        if (Tcl_DictObjGet(NULL, opts, kLevel, &v) == TCL_OK && v) { int l; if (Tcl_GetIntFromObj(NULL, v, &l) == TCL_OK) level = l; }
        if (Tcl_DictObjGet(NULL, opts, kCode, &v) == TCL_OK && v) { Tcl_GetIntFromObj(NULL, v, &code); }
        Tcl_DictObjPut(NULL, opts, kLevel, Tcl_NewIntObj(level > 1 ? (int) level - 1 : 0));
        Tcl_DictObjPut(NULL, opts, kCode, Tcl_NewIntObj(code));
        Tcl_SetReturnOptions(interp, opts);
        Tcl_DecrRefCount(opts); Tcl_DecrRefCount(kLevel); Tcl_DecrRefCount(kCode);
        return level > 1 ? TCL_RETURN : code;     /* "return -code break" stays a break, as from a proc */
    }
    if (rc == TCL_BREAK || rc == TCL_CONTINUE) {
        Tcl_SetObjResult(interp, Tcl_NewStringObj(rc == TCL_BREAK ? "invoked \"break\" outside of a loop" : "invoked \"continue\" outside of a loop", -1));
        return TCL_ERROR;
    }
    return rc;
}

/* kind: 0 calc=, 1 proc=, 2 method= */
static int CalcDefine(CalcState *st, Tcl_Interp *interp, int kind, Tcl_Size objc, Tcl_Obj *const objv[]) {
    static const char *const usage[3] = {"label body ?debug?", "name arglist body ?debug?", "name arglist body ?debug?"};
    static const char *const errFmt[3] = {"Calc= error in section '%s' %s", "Proc= error compiling '%s'  %s", "Method= error compiling '%s'  %s"};
    Tcl_Size base = kind == 0 ? 3 : 4;      /* index of the optional debug argument */
    Tcl_Obj *name = objv[1], *arglist = kind == 0 ? st->emptyObj : objv[2], *body = objv[base - 1];
    Tcl_Obj *debug = objc > base ? objv[base] : st->oneObj, *preserve, *newbody, *msg;
    int showDiff, rc;
    if (objc < base || objc > base + 1) { Tcl_WrongNumArgs(interp, 1, objv, usage[kind]); return TCL_ERROR; }
    if (DebugLevels(interp, debug, &preserve, &showDiff) != TCL_OK) return TCL_ERROR;

    newbody = Transform(st, interp, body, kind == 0 ? st->zeroObj : st->oneObj, preserve);
    if (newbody == NULL) {
        msg = Tcl_ObjPrintf(errFmt[kind], Tcl_GetString(name), Tcl_GetString(Tcl_GetObjResult(interp)));
        Tcl_DecrRefCount(preserve);
        Tcl_SetObjResult(interp, msg);
        Tcl_SetObjErrorCode(interp, Tcl_NewStringObj("NONE", -1));
        return TCL_ERROR;
    }
    if (kind == 0) {
        double d;
        if (Tcl_GetDoubleFromObj(interp, preserve, &d) != TCL_OK) { Tcl_DecrRefCount(preserve); Tcl_DecrRefCount(newbody); return TCL_ERROR; }
        if (d != 0) {
            Tcl_Obj *idx = Tcl_ObjPrintf("\"calc=%s\"", Tcl_GetString(name)), *vn = S("Calc::calc");
            Tcl_Obj *res;
            Tcl_IncrRefCount(idx); Tcl_IncrRefCount(vn);
            res = Tcl_ObjSetVar2(interp, vn, idx, newbody, TCL_LEAVE_ERR_MSG);
            Tcl_DecrRefCount(idx); Tcl_DecrRefCount(vn);
            if (res == NULL) { Tcl_DecrRefCount(preserve); Tcl_DecrRefCount(newbody); return TCL_ERROR; }
        }
    }
    Tcl_DecrRefCount(preserve);
    if (showDiff) {
        Tcl_Obj *dv[5];
        dv[0] = S("::Calc::debug");
        dv[1] = name;
        dv[2] = kind == 0 ? st->emptyObj : Tcl_ObjPrintf("{%s}", Tcl_GetString(arglist));
        dv[3] = body;
        dv[4] = newbody;
        Tcl_IncrRefCount(dv[0]);
        Tcl_IncrRefCount(dv[2]);
        /* looked up by name: the tests (and users) may replace Calc::debug */
        rc = Tcl_EvalObjv(interp, 5, dv, 0);
        Tcl_DecrRefCount(dv[0]);
        Tcl_DecrRefCount(dv[2]);
        if (rc != TCL_OK) { Tcl_DecrRefCount(newbody); return ProcLikeReturn(interp, rc); }
    }
    if (kind == 0) {
        rc = Tcl_EvalObjEx(interp, newbody, 0);
    } else {
        Tcl_Obj *av[4];
        av[0] = S(kind == 1 ? "proc" : "method");
        av[1] = name; av[2] = arglist; av[3] = newbody;
        Tcl_IncrRefCount(av[0]);
        rc = Tcl_EvalObjv(interp, 4, av, 0);
        Tcl_DecrRefCount(av[0]);
    }
    Tcl_DecrRefCount(newbody);
    return ProcLikeReturn(interp, rc);
}

static int CalcProcEq(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    return CalcDefine((CalcState *) cd, interp, 1, objc, objv);
}

static int CalcMethodEq(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    return CalcDefine((CalcState *) cd, interp, 2, objc, objv);
}

static int CalcCalcEq(void *cd, Tcl_Interp *interp, Tcl_Size objc, Tcl_Obj *const objv[]) {
    return CalcDefine((CalcState *) cd, interp, 0, objc, objv);
}

/* ------------------------------------------------------------------------------------
 * package init
 * ---------------------------------------------------------------------------------- */
static void CalcStateFree(void *cd, Tcl_Interp *interp) {
    CalcState *st = (CalcState *) cd;
    Tcl_DecrRefCount(st->v_inprocFlag);
    Tcl_DecrRefCount(st->v_tokens);
    Tcl_DecrRefCount(st->v_tokpos);
    Tcl_DecrRefCount(st->v_depth);
    Tcl_DecrRefCount(st->v_strictFlag);
    Tcl_DecrRefCount(st->v_labelcount);
    Tcl_DecrRefCount(st->nameRe);
    Tcl_DecrRefCount(st->emptyObj);
    Tcl_DecrRefCount(st->zeroObj);
    Tcl_DecrRefCount(st->colonObj);
    Tcl_DecrRefCount(st->peepRe);
    Tcl_DecrRefCount(st->reP1);
    Tcl_DecrRefCount(st->reP1b);
    Tcl_DecrRefCount(st->reComment);
    Tcl_DecrRefCount(st->reP2);
    Tcl_DecrRefCount(st->reP3a);
    Tcl_DecrRefCount(st->reP3ab);
    Tcl_DecrRefCount(st->reSuffix);
    Tcl_DecrRefCount(st->reP3);
    Tcl_DecrRefCount(st->reP3c);
    Tcl_DecrRefCount(st->reLoad);
    Tcl_DecrRefCount(st->oneObj);
    Tcl_DecrRefCount(st->v_cache);
    if (st->assembleName) Tcl_DecrRefCount(st->assembleName);
    Tcl_DecrRefCount(st->v_scratch);
    Tcl_DecrRefCount(st->v_inprec);
    Tcl_DecrRefCount(st->v_precode);
    Tcl_DecrRefCount(st->v_incode);
    Tcl_DecrRefCount(st->v_preprec);
    Tcl_DecrRefCount(st->v_nativeFunc);
    Tcl_Free((char *) st);
}

#ifdef _WIN32
__declspec(dllexport)
#endif
int Calcext_Init(Tcl_Interp *interp) {
#if TCL_MAJOR_VERSION >= 9
    if (Tcl_InitStubs(interp, "9.0", 0) == NULL) return TCL_ERROR;
#else
    if (Tcl_InitStubs(interp, "8.6", 0) == NULL) return TCL_ERROR;
#endif
    CalcState *st = (CalcState *) Tcl_Alloc(sizeof(CalcState));
    memset(st, 0, sizeof(*st));
    LOCAL_NAME(st->v_inprocFlag, "::Calc::inprocFlag");
    LOCAL_NAME(st->v_tokens, "::Calc::tokens");
    LOCAL_NAME(st->v_tokpos, "::Calc::tokpos");
    LOCAL_NAME(st->v_depth, "::Calc::depth");
    LOCAL_NAME(st->v_strictFlag, "::Calc::strictFlag");
    LOCAL_NAME(st->v_labelcount, "::labelcount");
    LOCAL_NAME(st->nameRe, "^[[:alpha:]_][[:alnum:]_]*$");
    LOCAL_NAME(st->emptyObj, "");
    LOCAL_NAME(st->zeroObj, "0");
    LOCAL_NAME(st->colonObj, ":");
    LOCAL_NAME(st->reP1, "^(\\s*)[=:]\\s+\\{");
    LOCAL_NAME(st->reP1b, "[=:]\\s+(\\{.*\\})");
    LOCAL_NAME(st->reComment, "\\s*;#[^\\n]*$");
    LOCAL_NAME(st->reP2, "^(\\s*)[=:]\\s+(\\S[^\\n]*)");
    LOCAL_NAME(st->reP3a, "\\[[=:]\\s+\\{");
    LOCAL_NAME(st->reP3ab, "\\[[=:]\\s+(\\{.*\\})");
    LOCAL_NAME(st->reSuffix, "\\}\\s*(\\].*)");
    LOCAL_NAME(st->reP3, "\\[\\s*[=:]\\s+[^\\]]+\\]");
    LOCAL_NAME(st->reP3c, "\\[\\s*[=:]\\s+([^\\]]+)\\]");
    LOCAL_NAME(st->reLoad, "push ([[:alpha:]_][^:;\\s]*); loadStk");
    LOCAL_NAME(st->oneObj, "1");
    LOCAL_NAME(st->v_cache, "::Calc::cache");
    LOCAL_NAME(st->v_scratch, "::Calc::_defaultVal");
    LOCAL_NAME(st->peepRe, "^push ([[:alpha:]_][^:;\\s]*); (.*); storeStk; $");
    LOCAL_NAME(st->v_inprec, "::Calc::inprec");
    LOCAL_NAME(st->v_precode, "::Calc::precode");
    LOCAL_NAME(st->v_incode, "::Calc::incode");
    LOCAL_NAME(st->v_preprec, "::Calc::preprec");
    LOCAL_NAME(st->v_nativeFunc, "::Calc::nativeFunc");

    CALC_CREATE(interp, "::Calc::incrCode", CalcIncrCode, st, NULL);

    CALC_CREATE(interp, "::Calc::patternText", CalcPatternText, st, NULL);

    CALC_CREATE(interp, "::Calc::AllProcsRecursive", CalcAllProcsRecursive, st, NULL);

    CALC_CREATE(interp, "::Calc::debug", CalcDebug, st, NULL);

    CALC_CREATE(interp, "::Calc::tokenise", CalcTokenise, st, NULL);

    CALC_CREATE(interp, "::Calc::parsePattern", CalcParsePattern, st, NULL);

    CALC_CREATE(interp, "::Calc::destructure", CalcDestructure, st, NULL);

    CALC_CREATE(interp, "::Calc::parseFuncArgs", CalcParseFuncArgs, st, NULL);

    CALC_CREATE(interp, "::Calc::parseTernary", CalcParseTernary, st, NULL);

    CALC_CREATE(interp, "::Calc::parsePrefix", CalcParsePrefix, st, NULL);

    CALC_CREATE(interp, "::Calc::parse", CalcParse, st, NULL);

    CALC_CREATE(interp, "::Calc::compile", CalcCompile, st, NULL);

    CALC_CREATE(interp, "::Calc::compile0", CalcCompile0, st, NULL);

    CALC_CREATE(interp, "::Calc::transform=", CalcTransform, st, NULL);

    CALC_CREATE(interp, "::Calc::transformProcs", CalcTransformProcs, st, NULL);

    CALC_CREATE(interp, "calc=", CalcCalcEq, st, NULL);
    CALC_CREATE(interp, "proc=", CalcProcEq, st, NULL);
    CALC_CREATE(interp, "::oo::define::method=", CalcMethodEq, st, NULL);
    CALC_CREATE(interp, ":", CalcColon, st, NULL);
    CALC_CREATE(interp, "=", CalcColon, st, NULL);

    /* the operator tables and settings that the Tcl module set up in namespace Calc */
    {
        static const char *const infix[][3] = {
            {")", "0", "-"}, {",", "0", "-"}, {"=", "0", "-"}, {"->", "0.5", "-"}, {"=>", "0.5", "-"},
            {"?", "1", "-"}, {":", "1", "-"}, {"||", "2", "-"}, {"&&", "3", "-"},
            {"|", "4", "bitor"}, {"^", "5", "bitxor"}, {"&", "6", "bitand"},
            {"==", "7", "eq"}, {"!=", "7", "neq"},
            {"<", "8", "lt"}, {">", "8", "gt"}, {"<=", "8", "le"}, {">=", "8", "ge"},
            {"<<", "9", "lshift"}, {">>", "9", "rshift"},
            {"+", "10", "add"}, {"-", "10", "sub"},
            {"*", "11", "mult"}, {"/", "11", "div"}, {"%", "11", "mod"}, {"**", "12", "expon"}
        };
        static const char *const prefix[][2] = {{"+", "uplus"}, {"-", "uminus"}, {"!", "not"}, {"~", "bitnot"}};
        static const char *const native[][2] = {
            {"strlen", "strlen 1"}, {"llength", "listLength 1"}, {"lindex", "lindexMulti -2"}, {"list", "list -1"},
            {"xstrcat", "strcat 2"}, {"xtoupper", "strcaseUpper 1"}, {"xtolower", "strcaseLower 1"},
            {"xstrindex", "strindex 2"}, {"xstrrange", "strrange 3"}
        };
        size_t k;
        for (k = 0; k < sizeof(infix) / sizeof(infix[0]); k++) {
            Tcl_SetVar2Ex(interp, "::Calc::inprec", infix[k][0], Tcl_NewStringObj(infix[k][1], -1), TCL_GLOBAL_ONLY);
            Tcl_SetVar2Ex(interp, "::Calc::incode", infix[k][0], Tcl_NewStringObj(infix[k][2], -1), TCL_GLOBAL_ONLY);
        }
        for (k = 0; k < sizeof(prefix) / sizeof(prefix[0]); k++)
            Tcl_SetVar2Ex(interp, "::Calc::precode", prefix[k][0], Tcl_NewStringObj(prefix[k][1], -1), TCL_GLOBAL_ONLY);
        for (k = 0; k < sizeof(native) / sizeof(native[0]); k++)
            Tcl_SetVar2Ex(interp, "::Calc::nativeFunc", native[k][0], Tcl_NewStringObj(native[k][1], -1), TCL_GLOBAL_ONLY);
        Tcl_SetVar2Ex(interp, "::Calc::preprec", NULL, Tcl_NewStringObj("13", -1), TCL_GLOBAL_ONLY);
        Tcl_SetVar2Ex(interp, "::Calc::strictFlag", NULL, Tcl_NewStringObj("0", -1), TCL_GLOBAL_ONLY);
    }
    if (Tcl_PkgProvide(interp, "colon", "1.0") != TCL_OK) return TCL_ERROR;

    Tcl_SetAssocData(interp, "calcext", CalcStateFree, st);
    return TCL_OK;
}
