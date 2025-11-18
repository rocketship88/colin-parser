#include <tcl.h>
#include <string.h>
#include <math.h>   // for floor()
#include <limits.h> // for LLONG_MIN, LLONG_MAX
// milestone 1 before cleanup of old code for expression.c


// INT_64 definition for cross-platform compatibility
#ifdef _WIN32
typedef __int64 INT_64;
#else
typedef long long INT_64;
#endif

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

// Helper: Compile expression to TAL (calls your Tcl code for now)
static Tcl_Obj* CompileExpression(Tcl_Interp *interp, Tcl_Obj *expr) {
    static Tcl_Obj *compileCmd = NULL;
    if (compileCmd == NULL) {
        compileCmd = Tcl_NewStringObj("Calc::compile0", -1);
        Tcl_IncrRefCount(compileCmd);
    }
    
    Tcl_Obj *objv[2];
    objv[0] = compileCmd;
    objv[1] = expr;
    
    int result = Tcl_EvalObjv(interp, 2, objv, 0);
    
    if (result != TCL_OK) {
        return NULL;
    }
    
    // CRITICAL: Duplicate result to protect from interpreter cleanup
    Tcl_Obj *talCode = Tcl_DuplicateObj(Tcl_GetObjResult(interp));
    return talCode;
}

// Helper: Execute assembled code
static int ExecuteAssemble(Tcl_Interp *interp, Tcl_Obj *talCode) {
    static Tcl_Obj *assembleCmd = NULL;
    if (assembleCmd == NULL) {
        assembleCmd = Tcl_NewStringObj("::tcl::unsupported::assemble", -1);
        Tcl_IncrRefCount(assembleCmd);
    }
    
    Tcl_Obj *objv[2];
    objv[0] = assembleCmd;
    objv[1] = talCode;
    
    int result = Tcl_EvalObjv(interp, 2, objv, 0);
    
    return result;
}
int CalcCmd(ClientData cd, Tcl_Interp *interp, 
            int objc, Tcl_Obj *const objv[]) {
    
    static Tcl_Obj *cacheVar = NULL;
    if (cacheVar == NULL) {
        cacheVar = Tcl_NewStringObj("::Calc::cache", -1);
        Tcl_IncrRefCount(cacheVar);
    }
    
    // Build expression - MUST increment refcount!
    Tcl_Obj *expr;
    if (objc == 2) {
        expr = objv[1];
        Tcl_IncrRefCount(expr);
    } else {
        expr = JoinArgs(objc, objv);
        Tcl_IncrRefCount(expr);  // Critical!
    }
    
    // Try cache lookup
    Tcl_Obj *talCode = Tcl_ObjGetVar2(interp, cacheVar, expr, TCL_GLOBAL_ONLY);
    
    int result;
    if (talCode != NULL) {
        // Cache hit
        result = ExecuteAssemble(interp, talCode);
    } else {
        // Cache miss - compile
        talCode = CompileExpression(interp, expr);
        if (talCode == NULL) {
            Tcl_DecrRefCount(expr);
            return TCL_ERROR;
        }
        
        Tcl_IncrRefCount(talCode);  // Keep it alive!
        Tcl_ObjSetVar2(interp, cacheVar, expr, talCode, TCL_GLOBAL_ONLY);
        result = ExecuteAssemble(interp, talCode);
        Tcl_DecrRefCount(talCode);
    }
    
    Tcl_DecrRefCount(expr);
    return result;
}
// Package initialization function
// Must be named Myext_Init to match the DLL name
#ifdef _WIN32
__declspec(dllexport)
#endif
int Calc_Init(Tcl_Interp *interp) {
    // Initialize Tcl stubs
#ifdef TCL9
    if (Tcl_InitStubs(interp, "9.0", 0) == NULL) {
#else
    if (Tcl_InitStubs(interp, "8.6", 0) == NULL) {
#endif
        return TCL_ERROR;
    }
    
    
   // Create the "::" command for comparing timing, also uses ::Calc::cache
    Tcl_CreateObjCommand(interp, "::", CalcCmd, NULL, NULL);

    // Provide the package
    if (Tcl_PkgProvide(interp, "calc", "1.0") != TCL_OK) {
        return TCL_ERROR;
    }
    
    return TCL_OK;
}
