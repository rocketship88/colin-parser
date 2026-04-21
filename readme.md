# Expression Evaluator with Array Support

This is Colin Macleod's bytecode-based expression evaluator for Tcl, with added support for array references and caching.

## What's New
- Repackaging into separate test file, and module
- proc= and method= to apply bytecodes directly, faster, and still pure tcl
- list is now a bytecode function, same as the gather mathfunc but faster
- Array support: `= myarray(index) * 2` and multi-dimensional `matrix(i,j)`
- Comprehensive test suite with 150+ verification tests

- Support for multiple expressions separated by ` ' ` or ` ; ` 
- Includes check for more than 1000 expressions in cache, to catch $var or [cmd] in expressions
- Wrapping in Calc:: namespace and some code cleanup

## Usage

```tcl
tclsh colon_test.tcl
wish  colon_test.tcl

Also:
cd <to location of extracted files>   
tclsh calc.test
```

The file will run all tests automatically and display results. This requires both `colon_test.tcl` and `colon-1.0.tm` be in the same directory.

## Use as a library

To use in a program: All the code is now in a `Tcl module`. Either place the module (`colon-1.0.tm`) into a known module directory, or use the `::tcl::tm::path add <path to module>` command. Then use `package require colon`.

## Use with proc= and method=

This is a way to parse text bytecodes via the assemble command during procedure or method compilation. This increases performance to effectively expr speed (within 5%). One writes and debugs code using the normal proc or method statement, but after debugged, and to get more performance, simply change `proc` to `proc=` and/or `method` to `method=` and it will generate a procedure/method body with the applied improvement. 

When used in this way, there is no need for the C extension, in fact, this method is 2-3x faster than using the C extension.

The proc= or method= code also has an option following the body of the procedure. It defaults to 1 which causes the assembly code to be wrapped in if {0} {... source code ...} {...assemble...} so that it does not execute, but is there in the event of an error traceback and to keep the line numbers correct. The bytecode compiler will optomize this out, so it shouldn't affect performance.

proc= and method= will also catch any $var or [command] substitutions in the expression and throw an error.

```tcl
::tcl::tm::path add [file dirname [info script]] ;# test file and module file in same directory
package require colon

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
Similarly, one can use method= with TCLOO methods:
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
## Features

- Bare variable syntax: `= x + y` (no $ needed)
- Array access: `= data(idx) + 100`
- Math functions: `= sqrt(x*x + y*y)`
- Ternary operator: `= x > 0 ? x : -x`
- Boolean operations: `= a < 10 && b > 5`
- Bignum support: `= 2 ** 1000`
- Bytecode caching for performance
- Assignment with `=` multiple statements with `'`  `;` or `newline` separators
- Right associative assignment, `= a = b = (3 + 4) * 2` ;# produces 14 -> a and b
- Can now cross lines if inside braces, no `;` needed
- For on line multiple statements without braces, can use `'` separator
- Command alias, `=` or `:` or choose your own (must use `:` or `=` when using `proc=`)
## Requirements

- Tcl 8.6 or 9.x
- tcllib (for fibonacci tests with math::fibonacci)

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

# use for multipe return values - can now use list(.,.,...) directly
proc tcl::mathfunc::gather {args} {
    return [list {*}$args]
}
set glist [: { x = sin(rad(90))**2; 
    y = cos(rad(90))**2;     
    z = sqrt(x) + sqrt(y);
    gather(x,y,z);
  }]
puts $glist
0.9999999999998932 1.0679490440337123e-13 1.0000003267948432  # output
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
### Using the C extension
Depending on which of the tcl or C extension one uses, the command is `:` or `::` in order to be able to compare the performance of pure tcl vs. the C extension.  Otherwise, the two commands produce the same results and both use the same `::cache` variable and call on the same compile and assemble commands for parsing and execution. The `::` name defined in the C code is easily changed to anything desired. The same goes for the tcl source. However, the test cases all expect the command to be `=`, and so there is an alias command just following the `:` proc definition at the top of the file to equate the two.
```tcl
interp alias {} = {} : ;# alias to = for test suite

load calc.dll   # windows 8.6
load calc9.dll  # windows 9.x

load calc.so    # linux

:: expression # uses the C extension 
:  expression # uses tcl command
=  expression # alias which all the test code uses
```
## Testing

All tests verify `=` command results against `expr` to ensure correctness.
