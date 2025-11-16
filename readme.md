# Expression Evaluator with Array Support

This is Colin Macleod's bytecode-based expression evaluator for Tcl, with added support for array references and caching.

## What's New

- Array support: `= myarray(index) * 2`
- fixes for boolean literals and multi-character function names (log10, atan2)
- Comprehensive test suite with 100+ verification tests

- Support for multiple expressions separated by ` ' ` ` ; ` or `newline`.
- Assignment operator, right associative, `a = b = c` possible too
- Allow for on-line comments with or without preceding ; as #... end of line
- Include check for more than 1000 expressions in cache, to catch $var in expressions
- Finish up with wrapping in Calc:: namespace and some code cleanup
- Preliminary upload of C extension for : command, 30% faster
## Usage

```tcl
tclsh colin.tcl
```

The file will run all tests automatically and display results. At about line 426 there are some knobs for testing. To use w/o tests, just remove all code below that point.

## Features

- Bare variable syntax: `= x + y` (no $ needed)
- Array access: `= data(idx) + 100`
- Math functions: `= sqrt(x*x + y*y)`
- Ternary operator: `= x > 0 ? x : -x`
- Boolean operations: `= a < 10 && b > 5`
- Bignum support: `= 2 ** 1000`
- Bytecode caching for performance
- New, assignment with `=` multiple statements with `'`  `;` or `newline` separators
- Right associative assignment, `= a = b = (3 + 4) * 2` ;# produces 14 -> a and b
- Can now cross lines if inside braces, no `;` needed
- For on line multiple statements without braces, can use `'` separator
- Command alias, `=` or `:` 
## Requirements

- Tcl 8.6 or 9.x
- tcllib (for fibonacci tests with math::fibonacci)

## Examples
```tcl
: {a = 1 ; b = 2}        ;# semicolon
: a = 1 ' b = 2          ;# single quote (no braces!)
: {a = 1                 ;# newline - note comment allowed here
   b = 2}

# custom functions
proc tcl::mathfunc::rad {deg} {
    return [expr {$deg * 3.14159265358979323846/180.}]
}

# use for multipe return values
proc tcl::mathfunc::gather {args} {
    return [list {*}$args]
}
set glist [: { x = sin(rad(90))**2 
    y = cos(rad(90))**2     
    z = sqrt(x) + sqrt(y)
    gather(x,y,z)
  }]
puts $glist
0.9999999999998932 1.0679490440337123e-13 1.0000003267948432  # output
```
## Building The C extension

### Windows (Visual Studio 2022)

Open "x64 Native Tools Command Prompt for VS 2022" and run:

**For Tcl 8.6:**
```batch
cl /O2 /Ob2 /Oi /Ot /GL  /DUSE_TCL_STUBS -IC:\Path\To\Tcl\include myext.c  ^
   /link -dll C:\Path\To\Tcl\lib\tclstub86.lib /OUT:myext.dll
```

**For Tcl 9.x:**
```batch
cl /O2 /Ob2 /Oi /Ot /GL  /DUSE_TCL_STUBS /DTCL9 -IC:\Path\To\Tcl\include myext.c  ^
   /link -dll C:\Path\To\Tcl\lib\tclstub.lib /OUT:myext9.dll
```

**Note:** 
- Replace `C:\Path\To\Tcl` with your Tcl installation path
- Tcl 9.x uses `tclstub.lib` (no version number) instead of `tclstub86.lib`

### Linux
```bash
gcc -O3 -march=native -flto -shared -fPIC -o myext.so myext.c  \
    -I/usr/include/tcl8.6 -ltclstub8.6
```

If Tcl headers are in a different location:
```bash
gcc -O3 -march=native -flto -shared -fPIC -o myext.so myext.c  \
    $(pkg-config --cflags tcl) -ltclstub8.6
```
### Using the C extension
Depending on which of the tcl or C extension one uses, the command is `:` or `::` in order to be able to compare the performance of pure tcl vs. the C extension.  Otherwise, the two commands produce the same results and both use the same `::cache` variable and call on the same compile and assemble commands for parsing and execution. The `::` name defined in the C code is easily changed to anything desired. The same goes for the tcl source. However, the test cases all expect the command to be `=`, and so there is an alias command just following the `:` proc definition at the top of the file to equate the two.
```tcl
interp alias {} = {} : ;# alias to = for test suite

load myext.dll   # windows 8.6
load myext9.dll  # windows 9.x

load myext.so    # linux

:: expression # uses the C extension 
:  expression # uses tcl command
=  expression # alias which all the test code uses
```
## Testing

All tests verify `=` command results against `expr` to ensure correctness.
