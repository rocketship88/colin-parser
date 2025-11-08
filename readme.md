# Expression Evaluator with Array Support

This is Colin Macleod's bytecode-based expression evaluator for Tcl, with added support for array references and caching.

## What's New

- Array support: `= myarray(index) * 2`
- fixes for boolean literals and multi-character function names (log10, atan2)
- Comprehensive test suite with 100+ verification tests

- Support for multiple expressions separated by ` ' ` ` ; ` or `newline`.
- Assignment operator, right associative, `a = b = c` possible too

## Usage

```tcl
tclsh colin.tcl
```

The file will run all tests automatically and display results.

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
: {a = 1                 ;# newline
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


## Testing

All tests verify `=` command results against `expr` to ensure correctness.
