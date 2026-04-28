# Calc: Expression Evaluator with Additional Features

This is Colin Macleod's original bytecode-based expression evaluator for Tcl, with added support for array references, native functions, multiple statements with assignment operator, bytecode caching, and proc and method compiling.

## What's New
- Repackaging into separate test file, and Tcl module
- proc= and method= to compile bytecodes directly, faster, while still just pure tcl
- calc= for toplevel compiling
- list is now a bytecode function instead of a mathfunc but faster
- Array support: `= myarray(index) * 2` and multi-dimensional `matrix(i,j)`
- Comprehensive test suite with 150+ verification tests

- Support for multiple expressions separated by ` ' ` or ` ; ` 
- Wrapping in Calc:: namespace
  
## Features

- Bare variable syntax: `= x + y` (no $ needed - and not allowed)
- Array access: `= data(idx) + 100`
- Math functions: `= sqrt(x*x + y*y)`
- Ternary operator: `= x > 0 ? x : -x`
- Boolean operations: `= a < 10 && b > 5`
- Bignum support: `= 2 ** 1000`
- Bytecode caching for performance
- Assignment with `=` multiple statements with `'` or `;`  separators
- Right associative assignment, `= a = b = (3 + 4) * 2` ;# produces 14 -> a and b
- Can now cross lines if inside braces, `;` needed
- For on line multiple statements without braces, can use `'` separator
- Command alias, `=` or `:` or choose your own (must use `:` or `=` when using `proc=` or `method=`)
- Included C extension provides up to 2x performance outside of procs and methods
  
## Requirements

- Tcl 8.6 or 9.x
- tcllib (for fibonacci tests with math::fibonacci)
  
## Run with test file

```tcl
tclsh colon_test.tcl
wish  colon_test.tcl

Also:
cd <to location of extracted files>   
tclsh calc.test
```

The file will run all tests automatically and display results. This requires both `colon_test.tcl` and `colon-1.0.tm` be in the same directory.

## Use as a library

All the code is now in a `Tcl module`. Either place the module `colon-1.0.tm` into a known module directory, or use the following command to add a directory path, similar to using auto_path for regular packages.
```
::tcl::tm::path add <path to module>
package require colon 1.0
```
## Compiling with `proc=` and `method=`

Changing `proc` to `proc=` or `method` to `method=` compiles expressions once at definition time, achieving performance identical to hand-written `set`/`expr` code.

A typical workflow is to develop and debug using normal `proc`/`method`, then switch to `proc=`/`method=` once the code is working. Debugging tools such as the TclPro debugger can be used with the original source, then `proc=`/`method=` applied for production performance.

An optional argument (default 1) controls source preservation — when enabled, the original source is wrapped in `if {0} {...}` alongside the assembled code, keeping line numbers correct for error reporting. The bytecode compiler eliminates the dead branch entirely so there is no performance cost.

The C extension complements `proc=`/`method=` rather than replacing it — `proc=`/`method=` handles code inside procedures and methods, while the C extension improves performance for expressions evaluated outside of them. Both can be used together since `proc=`/`method=` generates only assembly code and the C extension never comes into play for compiled procs or methods.

Any attempt to use `$var` or `[command]` substitutions inside a `:` or `=` expression is caught at definition time with a clear error message.

## Toplevel Compilation with `calc=`

`calc=` transforms a block of code containing `:` and `=` expressions and executes it in the caller's scope. Use it to wrap toplevel initialization and setup code for cleaner syntax. For performance-critical loop bodies, use `proc=` instead. Using `calc=` eliminates the need for a cache since it directly replaces `:` or `=` calls with TAL code. It includes a label parameter to aid in error line number messaging since it can be used in several distinct source code sections.

```tcl
calc= setup {
    : pi = 3.14159265358979
    : twopi = pi * 2
    : halfpi = pi / 2
    puts "pi=[: pi] twopi=[: twopi]"
}
```

## A look Under the Hood

To illustrate how proc= works, here is a simple canvas helper that tags a rectangular region. The three views — source, generated assembly, and disassembled bytecode — show how clean source code translates to optimal execution with no extra steps.

```
proc= box {x y delta tag}  {.canvas addtag $tag {*}[: list(x-delta, x+delta, y-delta, y+delta) ]}

proc  box {x y delta tag}  {.canvas addtag $tag {*}[tcl::unsupported::assemble {load x; load delta; sub;
    load x; load delta; add; load y; load delta; sub; load y; load delta; add; list 4; }]
}

ByteCode 0x00000000081B44B0, refCt 1, epoch 17, interp 0x00000000009AFBD0 (epoch 17)
  Source ".canvas addtag $tag {*}[tcl::unsupported::assemble {loa..."
  Cmds 2, src 163, inst 48, litObjs 2, aux 0, stkDepth 8, code/src 0.00
  Proc 0x0000000005547E00, refCt 1, args 4, compiled locals 4
      slot 0, scalar, arg, "x"
      slot 1, scalar, arg, "y"
      slot 2, scalar, arg, "delta"
      slot 3, scalar, arg, "tag"
  Commands 2:
      1: pc 0-46, src 0-161        2: pc 7-40, src 24-160
  Command 1: ".canvas addtag $tag {*}[tcl::unsupported::assemble {loa..."
    (0) expandStart 
    (1) push1 0 	# ".canvas"
    (3) push1 1 	# "addtag"
    (5) loadScalar1 %v3 	# var "tag"
  Command 2: "tcl::unsupported::assemble {load x; load delta; sub; lo..."
    (7) startCommand +34 1 	# next cmd at pc 41, 1 cmds start here
    (16) loadScalar1 %v0 	# var "x"
    (18) loadScalar1 %v2 	# var "delta"
    (20) sub 
    (21) loadScalar1 %v0 	# var "x"
    (23) loadScalar1 %v2 	# var "delta"
    (25) add 
    (26) loadScalar1 %v1 	# var "y"
    (28) loadScalar1 %v2 	# var "delta"
    (30) sub 
    (31) loadScalar1 %v1 	# var "y"
    (33) loadScalar1 %v2 	# var "delta"
    (35) add 
    (36) list 4 
    (41) expandStkTop 4 
    (46) invokeExpanded 
    (47) done 

```

##  Example with performance comparisons

The rounded rectangle example below demonstrates real-world performance. Three versions were benchmarked with 1000 iterations:

- **`proc=` version** — matches `expr`/`set` performance exactly, within the margin of error of the `time` command
- **C extension version** — 2x slower overall, 4x slower for pure calculation
- **Uncompiled `:` version** — 3x slower overall, 8x slower for pure calculation

The overall timings include polygon drawing, which dominates and masks the calculation differences. When timing only the mathematical calculations, `proc=` and hand-written `expr`/`set` are indistinguishable, while the gap to the other versions becomes more apparent.

This demonstrates that `proc=` eliminates all expression evaluation overhead — the generated bytecode is identical to what Tcl produces from hand-written `set` and braced `expr` commands.

```tcl
::tcl::tm::path add [file dirname [info script]] ;# test file and module file in same directory
package require colon

# Derived from a tclcore email by Florent Merlet
 proc= roundRect { w x0 y0 x3 y3 radius args } {
    set r      [winfo pixels $w $radius]
    :   d    = 2*r
    :   maxr = 0.75
    if  {[: d > maxr * ( x3 - x0 )]} {
          : d = maxr * ( x3 - x0 )
    }
    if  {[: d > maxr * ( y3 - y0 )]}  {
          : d = maxr * ( y3 - y0 )
    }    
    : { x1 = x0 + d;
        x2 = x3 - d;
        y1 = y0 + d;
        y2 = y3 - d;       
        Poly = list(
            x0, y0, x1, y0, x2, y0, 
            x3, y0, x3, y1, x3, y2, 
            x3, y3, x2, y3, x1, y3, 
            x0, y3, x0, y2, x0, y1
            )
      }
    $w create polygon {*}$Poly -smooth 1  {*}$args
} 0  ;# defaults to 1, includes original source, 0 to suppress

 grid [canvas .c -width 600 -height 300]
 grid [scale .s -orient horizontal \
          -label "Radius" \
          -variable rad -from 0 -to 200 \
          -command doit] -sticky ew

 proc doit { args } {
    global rad
    .c delete rect
    roundRect .c 100 50 500 250 $rad -fill white -outline black -tags rect
 }

```
## Example method= with TCLOO methods:
```
oo::class create Calculator {
    variable result
    
    constructor {} {
        set result 0
    }
    
    method= compute {x y} {
        : result = sqrt(x*x + y*y)
        return $result
    }
    
    method getResult {} {
        return $result
    }
}

set calc [Calculator new]
puts [$calc compute 3 4]   ;# should print 5.0
puts [$calc getResult]      ;# should print 5.0

```


## Examples
```tcl
: {a = 1 ; b = 2}        ;# semicolon
: a = 1 ' b = 2          ;# single quote (no braces!)
: {a = 1 ;               ;# newline - note comment allowed here
   b = 2}

# custom functions
proc tcl::mathfunc::rad {deg} {
    return [expr {$deg * 3.14159265358979323846/180.}]
}

# use list() for multipe return values

set glist [: { x = sin(rad(90))**2; 
    y = cos(rad(90))**2;     
    z = sqrt(x) + sqrt(y);
    list(x,y,z);
  }]
puts $glist
# 1.0 3.749399456654644e-33 1.0 # output
```
## Building The C extension

### Windows (Visual Studio 2022)

Open "x64 Native Tools Command Prompt for VS 2022" and run:

**For Tcl 8.6:**
```batch
cl /O2 /Ob2 /Oi /Ot /GL  /DUSE_TCL_STUBS -IC:\Path\To\Tcl\include calc.c  ^
   /link -dll C:\Path\To\Tcl\lib\tclstub86.lib /OUT:calc.dll
```

**For Tcl 9.x:**
```batch
cl /O2 /Ob2 /Oi /Ot /GL  /DUSE_TCL_STUBS /DTCL9 -IC:\Path\To\Tcl\include calc.c  ^
   /link -dll C:\Path\To\Tcl\lib\tclstub.lib /OUT:calc9.dll
```

**Note:** 
- Replace `C:\Path\To\Tcl` with your Tcl installation path
- Tcl 9.x uses `tclstub.lib` (no version number) instead of `tclstub86.lib`

### Linux
```bash
gcc -O3 -march=native -flto -shared -fPIC -o calc.so calc.c  \
    -I/usr/include/tcl8.6 -ltclstub8.6
```

If Tcl headers are in a different location:
```bash
gcc -O3 -march=native -flto -shared -fPIC -o calc.so calc.c  \
    $(pkg-config --cflags tcl) -ltclstub8.6
```
### Using the C Extension

The C extension provides the `::` command, which is functionally identical to the pure Tcl `:` and `=` commands — the Tcl module defines both as aliases for the same implementation. All three use the same `::Calc::cache`, and the same assembler. The C extension is faster in several ways: it uses a single C call for the cache lookup rather than two Tcl commands, and it joins multiple arguments into a single expression string more efficiently than the pure Tcl version. It also caches the pointer to the assemble command avoiding a runtime string lookup. These are enough to gain 2x or more in performance.

To load the appropriate version and switch the `=` alias to use the C extension:

```tcl
if { $tcl_version >= "9.0" } {
    load calc9.dll  ;# Windows 9.x
} else {
    load calc.dll   ;# Windows 8.6
}
# load calc.so     ;# Linux

# By default both = and : use the pure Tcl implementation
# Uncomment to switch = to the C extension:
# rename = old=
# rename :: =

```

The command names can be changed to anything — the test suite uses `=` throughout, and `rename` makes it easy to switch implementations without changing any other code.

## Testing

All tests verify `=` command results against `expr` to ensure correctness.
