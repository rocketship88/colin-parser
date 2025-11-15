# Calc Expression Evaluator - User Manual

## Introduction

Calc is a high-performance expression evaluator for Tcl that compiles expressions to Tcl Assembly Language (TAL) bytecode. It provides a clean syntax for mathematical calculations with support for variables, arrays, functions, and multiple statements.

The code is contained in the file colin.tcl that in the delivered form is in two parts, the code at the top and a set of test cases at the bottom. The user should copy the top portion into a file, for example, Calc-1.1.tm and then it can be used as a module. Alternatively, it can simply be sourced into a program. If the C extension is used, then it must be built from source and then it is loaded using the tcl load command. The C extension is optional.

## Basic Usage

```tcl
# Load the Calc package
package require Calc

# Simple calculation
: 2 + 3 * 4
# Returns: 14

# Using variables
set x 10
set y 20
: x + y
# Returns: 30

# Complex expression
: (x + y) * 2 - x / 2
# Returns: 55
```

## Command Syntax

The Calc evaluator uses the `:` command (or `=` in some versions):

```tcl
: expression
```

All expressions are evaluated without `$` variable substitution or `[command]` substitution.

## Operators

Calc supports standard operators with C-like precedence:

**Arithmetic:**
- `+` `-` `*` `/` `%` (modulo)
- `**` (exponentiation)

**Bitwise:**
- `&` `|` `^` (and, or, xor)
- `~` (bitwise not)
- `<<` `>>` (left shift, right shift)

**Comparison:**
- `==` `!=` `<` `>` `<=` `>=`

**Logical:**
- `&&` `||` `!`

**Ternary:**
- `condition ? true_value : false_value`

**Assignment:**
- `=` (right associative)

### Operator Precedence (lowest to highest)

1. `,` `)` `=` (precedence 0)
2. `?` `:` (ternary)
3. `||`
4. `&&`
5. `|`
6. `^`
7. `&`
8. `==` `!=`
9. `<` `>` `<=` `>=`
10. `<<` `>>`
11. `+` `-`
12. `*` `/` `%`
13. `**` (right associative)
14. Unary: `+` `-` `!` `~`

## Variables

Variables are referenced **without** the `$` prefix:

```tcl
set x 100
set y 50

# CORRECT - no $ needed
: x + y
# Returns: 150

# WRONG - this would defeat performance and possibly overflow the cache
: $x + $y
```

### Why No `$` Substitution?

Calc does **not** use Tcl's variable substitution. Instead, variables are loaded at runtime via bytecode instructions. This enables:

- Better caching (same bytecode works with different values)
- Faster execution (no string substitution overhead)
- Cleaner syntax

**If you need `$var` or `[command]` substitution, use Tcl's built-in `expr` instead:**

```tcl
# With Calc - variables by name
set x 10
: x * 2
# Returns: 20

# With expr - substitution happens first
set x 10
expr {$x * 2}
# Returns: 20

# Command substitution - use expr
expr {[llength $mylist] + 5}

# Or define a mathfunc for Calc
proc tcl::mathfunc::listlen {lst} {
    return [llength $lst]
}
: listlen(mylist) + 5

# See below, since llength, and lindex are available natively
```

## Assignments

The `=` operator is **right associative** and can be chained:

```tcl
# Single assignment
: x = 42
# x is now 42

# Chained assignments (right to left)
: a = b = c = 100
# All three variables are now 100

# Assignment with expression
: result = x * 2 + y / 3

# Assignment returns the value
set z [: total = x + y + 10]
# total and z both contain the result
```

**Important:** You can only assign to **variables**, not to:
- Number literals: `: 10 = 5` → Error
- Boolean literals: `: true = 3` → Error
- Array elements (see Arrays section below)

## Arrays

Arrays can be read using function-style syntax:

```tcl
array set data {
    apple  10
    banana 20
    cherry 30
}

set key apple
: data(key) + 5
# Returns: 15

# Literal indices work too
: data(banana) * 2
# Returns: 40
```

### Multi-Dimensional Arrays

Multi-dimensional arrays use comma-separated indices. **The Calc parser handles spaces fine,** but when **creating** the array in Tcl, you must use **no spaces** in the index keys:

```tcl
# CORRECT - no spaces in array keys when setting values
array set matrix {
    0,0 100   0,1 101   0,2 102
    1,0 200   1,1 201   1,2 202
    2,0 300   2,1 301   2,2 302
}

# Also correct
set matrix(0,0) 100
set matrix(0,1) 101

# WRONG - spaces in keys create different indices
set matrix(0, 0) 100  # This creates key "0, 0" (with space!)
set matrix(0,0)  100  # This creates key "0,0" (no space)
# These are DIFFERENT array elements!
```

**Using multi-dimensional arrays in Calc:**

```tcl
set i 1
set j 2

# Spaces in Calc expressions are fine
: matrix(i, j)      # Works - returns 202
: matrix(i,j)       # Also works - returns 202
: matrix( i , j )   # Also works - returns 202

# Three dimensions
array set cube {
    0,0,0 1000   0,0,1 1001
    1,0,0 2000   1,1,1 2111
}

set x 1
set y 1
set z 1
: cube(x, y, z)
# Returns: 2111

# Expressions as indices
: matrix(i+1, j-1)
```

**Note:** The comma separator generates a composite string index internally (e.g., `"1,2"`), which is how Tcl represents multi-dimensional array indices. Calc builds this string at runtime by concatenating the index values with commas.

### Array Assignment Limitation

**Arrays are read-only in Calc.** You cannot assign to array elements:

```tcl
# WRONG - not supported in Calc
: data(key) = 100

# CORRECT - use regular Tcl
set data(key) 100
```

This is because array assignment requires different bytecode (`storeArrayStk` with special handling) that conflicts with the function/array syntax ambiguity.

## Functions

Calc supports two types of functions:

### 1. Native Bytecode Functions (Fastest)

These compile directly to TAL instructions:

```tcl
set text "Hello, World!"
set mylist {a b c d e}

# String length
: strlen(text)
# Returns: 13

# List length
: llength(mylist)
# Returns: 5

# List indexing (single index)
: lindex(mylist, 2)
# Returns: "c"

# List indexing (nested lists)
set matrix {{1 2 3} {4 5 6} {7 8 9}}
: lindex(matrix, 1, 2)
# Returns: 6

# Multiple indices for deep nesting
set cube {{{a b} {c d}} {{e f} {g h}}}
: lindex(cube, 1, 0, 1)
# Returns: "f"
```

**Available native functions:**
- `strlen(string)` - String length
- `llength(list)` - List length  
- `lindex(list, index, ...)` - List element access (variable arguments)

### 2. Math Functions (via tcl::mathfunc)

Define custom functions in the `tcl::mathfunc` namespace:

```tcl
# Define a custom function
proc tcl::mathfunc::square {x} {
    expr {$x * $x}
}

: square(5)
# Returns: 25

# More complex example
proc tcl::mathfunc::distance {x1 y1 x2 y2} {
    set dx [expr {$x2 - $x1}]
    set dy [expr {$y2 - $y1}]
    return [expr {sqrt($dx*$dx + $dy*$dy)}]
}

set px 10
set py 20
set qx 13
set qy 24
: distance(px, py, qx, qy)
# Returns: 5.0
```

**Standard math functions available:**
- `abs()` `ceil()` `floor()` `round()` `int()`
- `sqrt()` `pow()` `exp()` `log()` `log10()`
- `sin()` `cos()` `tan()` `asin()` `acos()` `atan()` `atan2()`
- `sinh()` `cosh()` `tanh()`
- `min()` `max()` (variable arguments)
- And others defined in `tcl::mathfunc::*`

### The gather() Function

The `gather()` function collects multiple values into a list:

```tcl
proc tcl::mathfunc::gather {args} {
    return [list {*}$args]
}

: gather(1, 2, 3)
# Returns: {1 2 3}

set x 10
set y 20
: gather(x, y, x+y)
# Returns: {10 20 30}

# Useful for returning multiple calculated values
: gather(min(x,y), max(x,y), x+y, x*y)
# Returns: {10 20 30 200}
```

## Multiple Statements

Calc supports multiple statements separated by:
- Semicolon (`;`)
- Newline
- Both

### Using Semicolons

```tcl
: x = 10 ; y = 20 ; x + y
# Returns: 30 (result of last expression)

# Each statement is executed in order
: a = 5 ; b = a * 2 ; c = a + b
# a=5, b=10, c=15, returns 15
```

### Using Newlines

```tcl
: {
    x = 10
    y = 20
    x + y
}
# Returns: 30
```

### Mixed Separators

```tcl
: {
    x = 10 ; y = 20
    z = x + y
    
    result = z * 2
}
# Returns: 60
```

**Blank lines are allowed** and ignored.

## Comments

Comments can be added using `#`:

```tcl
# Full-line comment
: x = 10  # End-of-line comment

# Multi-statement with comments
: {
    x = 10      # Initialize x
    y = 20      # Initialize y
    z = x + y   # Calculate sum
}

# Inline comments after statements need ;# in some contexts since tcl will process the ; in this case
: a = 5 ;# Set a to 5
```

## Non-Numeric Results

While Calc is designed for numerical calculations, some operations can return non-numeric values:

### Lists from lindex()

```tcl
set matrix {{1 2 3} {4 5 6} {7 8 9}}

# Single index returns a list
: lindex(matrix, 1)
# Returns: {4 5 6}

# You typically wouldn't use this in further math:
# : lindex(matrix, 1) + 10  # Probably not what you want
```

### Strings from Arrays

```tcl
array set data {
    name "Alice"
    age  30
}

: data(name)
# Returns: "Alice"

# Similarly, not useful for math operations
```

### Lists from gather()

```tcl
: gather(10, 20, 30)
# Returns: {10 20 30}

# gather() is typically used for collecting results,
# not for further calculations
```

**Best practice:** Use these operations when you need to return or collect values, but the results are generally the final output, not intermediate values for further math.

## Performance Notes

### Caching

Calc caches compiled bytecode for extremely fast re-execution:

```tcl
# First execution compiles and caches
: x * 2 + y / 3

# Subsequent executions use cached bytecode
# (even with different values of x and y)
: x * 2 + y / 3  # Very fast!
```

### Cache Overflow Detection

If you accidentally use `$var` substitution, it creates a new cache entry for each value:

```tcl
# WRONG - creates cache bloat
for {set i 0} {$i < 1000} {incr i} {
    : $i + 1  # Creates 1000 cache entries!
}
# Error: Cache overflow detected

# CORRECT - single cache entry
for {set i 0} {$i < 1000} {incr i} {
    : i + 1  # Same bytecode, different values
}
```

This is a **feature**, not a bug - it helps you identify performance problems early.

## Complete Examples

### Example 1: Quadratic Formula

```tcl
proc tcl::mathfunc::sqrt {x} {
    expr {sqrt($x)}
}

# Solve ax² + bx + c = 0
set a 1
set b -5
set c 6

: {
    discriminant = b*b - 4*a*c
    root1 = (-b + sqrt(discriminant)) / (2*a)
    root2 = (-b - sqrt(discriminant)) / (2*a)
    gather(root1, root2)
}
# Returns: {3.0 2.0}
```

### Example 2: Statistical Calculations

```tcl
proc tcl::mathfunc::sqrt {x} { expr {sqrt($x)} }

set data {10 20 30 40 50}

: {
    n = listlen(data)
    sum = lindex(data,0) + lindex(data,1) + lindex(data,2) + 
          lindex(data,3) + lindex(data,4)
    mean = sum / n
    gather(n, sum, mean)
}
# Returns: {5 150 30}
```

### Example 3: Matrix Operations

```tcl
array set mat {
    0,0 1  0,1 2  0,2 3
    1,0 4  1,1 5  1,2 6
    2,0 7  2,1 8  2,2 9
}

set i 1
set j 1

: {
    center = mat(i,j)
    top    = mat(i-1,j)
    bottom = mat(i+1,j)
    left   = mat(i,j-1)
    right  = mat(i,j+1)
    sum = center + top + bottom + left + right
}
# Returns: 35 (5+2+8+4+6)
```

### Example 4: Financial Calculation

```tcl
proc tcl::mathfunc::pow {x y} { expr {pow($x,$y)} }

# Compound interest: A = P(1 + r/n)^(nt)
set principal 1000
set rate 0.05
set compounds_per_year 12
set years 10

: {
    r_over_n = rate / compounds_per_year
    exponent = compounds_per_year * years
    amount = principal * pow(1 + r_over_n, exponent)
}
# Returns: ~1647.01
```


## Differences from expr

| Feature | Calc (`:`) | Tcl `expr` |
|---------|-----------|-----------|
| Variable reference | `x` | `$x` |
| Command substitution | Not supported | `[cmd]` |
| Multiple statements | `;` or newline | Not supported |
| Comments | `#` supported | Not in expression |
| Array syntax | `arr(key)` | `$arr(key)` |
| Performance | Cached bytecode | Compiled per call |
| Assignment | `x = value` | `set x [expr ...]` |

## Summary

Calc provides:
- ✅ Fast bytecode compilation and caching
- ✅ Clean syntax without `$` sigils
- ✅ Multiple statements and comments
- ✅ Right-associative assignment chains
- ✅ Native array and list operations
- ✅ Extensible via `tcl::mathfunc::*`
- ✅ Performance bug detection (cache overflow)

**When to use Calc:**
- Mathematical calculations with variables
- Multiple related calculations
- Performance-critical expression evaluation
- Clean, readable calculation code

**When to use expr:**
- Need `$var` or `[command]` substitution
- Single, simple expressions
- Standard Tcl scripting

---

*For more information, see the Calc source code and test suite.*
