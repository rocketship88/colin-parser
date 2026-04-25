#include <tcl.h>
#include <string.h>
#include <math.h>   // for floor()
#include <limits.h> // for LLONG_MIN, LLONG_MAX


/*

F:\myext>cl /O2 /Zi /Ob2 /Oi /Ot /GL /DUSE_TCL_STUBS -IC:\Users\core5\AppData\Local\Apps\Tcl86\include calc.c  /link -dll /DEBUG /LTCG /OPT:REF /OPT:ICF C:\Users\core5\AppData\Local\Apps\Tcl86\lib\tclstub86.lib /OUT:calc.dll

nmake /f makefile debug
nmake /f makefile release

nmake /f makefile9 debug 
nmake /f makefile9 release

*/

// INT_64 definition for cross-platform compatibility
#ifdef _WIN32
#include <intrin.h> // for __debugbreak()
typedef __int64 INT_64;
#else
typedef long long INT_64;
#endif

// Per-interpreter state: assemble dispatch cache + cache array name
typedef struct {
    Tcl_ObjCmdProc *assembleProc;   // resolved once per interpreter
    ClientData      assembleCD;
    Tcl_Obj        *cmdNameObj;     // "::tcl::unsupported::assemble"
    Tcl_Obj        *cacheVar;       // "::Calc::cache"
} CalcState;

// Called when the command is deleted (interpreter teardown or explicit rename/delete)
static void CalcDeleteProc(ClientData cd) {
    CalcState *state = (CalcState *)cd;
    if (state->cmdNameObj) Tcl_DecrRefCount(state->cmdNameObj);
    if (state->cacheVar)   Tcl_DecrRefCount(state->cacheVar);
    // assembleProc/assembleCD are not owned by us, no cleanup needed
    ckfree((char *)state);
}

// Helper: Join multiple arguments into single string
static Tcl_Obj* JoinArgs(int objc, Tcl_Obj *const objv[]) {
    Tcl_Obj *result = Tcl_NewObj();
    for (int i = 1; i < objc; i++) {
        if (i > 1) {
            Tcl_AppendToObj(result, " ", 1);
        }
        Tcl_AppendObjToObj(result, objv[i]);
    }
    return result;
}

// Helper: Compile expression to TAL (calls Tcl-level Calc::compile0)
static Tcl_Obj* CompileExpression(Tcl_Interp *interp, Tcl_Obj *expr) {
    static Tcl_Obj *compileCmd = NULL;
    if (compileCmd == NULL) {
        compileCmd = Tcl_NewStringObj("Calc::compile0", -1);
        Tcl_IncrRefCount(compileCmd);
    }

    Tcl_Obj *objv[2];
    objv[0] = compileCmd;
    objv[1] = expr;

    if (Tcl_EvalObjv(interp, 2, objv, 0) != TCL_OK) {
        return NULL;
    }

    // Duplicate result to protect from interpreter result cleanup
    return Tcl_DuplicateObj(Tcl_GetObjResult(interp));
}

// Helper: Simplest version - EvalObjv with cached command name string
// Process-wide static is safe here: only caches a plain string Tcl_Obj
static int ExecuteAssemblex(Tcl_Interp *interp, Tcl_Obj *talCode) {
    static Tcl_Obj *assembleCmd = NULL;
    if (assembleCmd == NULL) {
        assembleCmd = Tcl_NewStringObj("::tcl::unsupported::assemble", -1);
        Tcl_IncrRefCount(assembleCmd);
    }

    Tcl_Obj *objv[2];
    objv[0] = assembleCmd;
    objv[1] = talCode;

    return Tcl_EvalObjv(interp, 2, objv, 0);
}

// Helper: Execute TAL code via cached per-interpreter assemble dispatch
static int ExecuteAssemble(CalcState *state, Tcl_Interp *interp, Tcl_Obj *talCode) {
    // One-time resolution per interpreter
    if (state->assembleProc == NULL) {
        Tcl_CmdInfo info;
        if (!Tcl_GetCommandInfo(interp, "::tcl::unsupported::assemble", &info)) {
            Tcl_SetResult(interp,
                "cannot resolve ::tcl::unsupported::assemble", TCL_STATIC);
            return TCL_ERROR;
        }
        if (!info.isNativeObjectProc) {
            Tcl_SetResult(interp,
                "::tcl::unsupported::assemble has no objProc", TCL_STATIC);
            return TCL_ERROR;
        }
        state->assembleProc = info.objProc;
        state->assembleCD   = info.objClientData;

        state->cmdNameObj = Tcl_NewStringObj("::tcl::unsupported::assemble", -1);
        Tcl_IncrRefCount(state->cmdNameObj);
    }

    Tcl_Obj *objv[2] = { state->cmdNameObj, talCode };
    return state->assembleProc(state->assembleCD, interp, 2, objv);
}

int CalcCmd(ClientData cd, Tcl_Interp *interp,
            int objc, Tcl_Obj *const objv[]) {

    CalcState *state = (CalcState *)cd;

    // Build expression string from one or more arguments
    Tcl_Obj *expr;
    if (objc == 2) {
        expr = objv[1];
        Tcl_IncrRefCount(expr);
    } else {
        expr = JoinArgs(objc, objv);
        Tcl_IncrRefCount(expr);
    }

    // Try cache lookup — one call, both existence test and retrieval
    Tcl_Obj *talCode = Tcl_ObjGetVar2(interp, state->cacheVar, expr, TCL_GLOBAL_ONLY);

    int result;
    if (talCode != NULL) {
        // Cache hit
        result = ExecuteAssemble(state, interp, talCode);
    } else {
        // Cache miss - compile and store
        talCode = CompileExpression(interp, expr);
        if (talCode == NULL) {
            Tcl_DecrRefCount(expr);
            return TCL_ERROR;
        }

        Tcl_IncrRefCount(talCode);
        Tcl_ObjSetVar2(interp, state->cacheVar, expr, talCode, TCL_GLOBAL_ONLY);
        result = ExecuteAssemble(state, interp, talCode);
        Tcl_DecrRefCount(talCode);
    }

    Tcl_DecrRefCount(expr);
    return result;
}

#ifdef _WIN32
__declspec(dllexport)
#endif
int Calc_Init(Tcl_Interp *interp) {
//    __debugbreak();  // VS will halt here as soon as the DLL loads
#ifdef TCL9
    if (Tcl_InitStubs(interp, "9.0", 0) == NULL) {
#else
    if (Tcl_InitStubs(interp, "8.6", 0) == NULL) {
#endif
        return TCL_ERROR;
    }

    // Allocate per-interpreter state
    CalcState *state = (CalcState *)ckalloc(sizeof(CalcState));
    state->assembleProc = NULL;
    state->assembleCD   = NULL;
    state->cmdNameObj   = NULL;

    state->cacheVar = Tcl_NewStringObj("::Calc::cache", -1);
    Tcl_IncrRefCount(state->cacheVar);

    // "::" command - also uses ::Calc::cache, for timing comparisons
    Tcl_CreateObjCommand(interp, "::", CalcCmd, (ClientData)state, CalcDeleteProc);

    if (Tcl_PkgProvide(interp, "calc", "1.0") != TCL_OK) {
        return TCL_ERROR;
    }

    return TCL_OK;
}
