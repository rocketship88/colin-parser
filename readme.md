# Expression Evaluator with Array Support

This is Colin Macleod's bytecode-based expression evaluator for Tcl, with added support for array references and caching.

## What's New

- Array support: `= myarray(index) * 2`
- fixes for boolean literals and multi-character function names (log10, atan2)
- Comprehensive test suite with 100+ verification tests

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

## Requirements

- Tcl 8.6 or 9.x
- tcllib (for fibonacci tests with math::fibonacci)


## Testing

All tests verify `=` command results against `expr` to ensure correctness.
