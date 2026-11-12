# Behavior found while writing the tests

Writing tests for the mutants that survived turned up the following. Items 1 and 2 were fixed in
`colon-1.0.tm` afterwards; the others were left as they are.

1. **Nested ternary without parentheses (fixed).** `a ? b : c ? d : e` used to be read as
   `(a ? b : c) ? d : e`, and `a ? b ? c : d : e` was an error. The `?` operator is now right
   associative, as in `expr` (tests calc-45.7 and calc-55.1 to 55.3).
2. **`&&` and `||` evaluated both sides (fixed).** `= 1 || 1/0` used to give `divide by zero`.
   They now short circuit like `expr` and compile to the same instructions (tests calc-45.8 and
   calc-54.1 to 54.15). This was found by mutation testing and fixed afterwards.
3. **Text between `}` and `]` is dropped.** In `set v [: { x + 1 } ; set k ]`, the preprocessor
   keeps only `set v [assemble {...}]` and loses `; set k`. This is the fallback branch for the
   closing bracket (survivors #818 to #826), which is why those are left alone: a test would
   cement the loss.
4. **A missing operand is found at run time.** `= 1 +` and `= q =` compile (also inside `proc=`,
   `calc=` and `method=`), and the error appears when the code first runs, as
   `wrong # args: should be "push value"`. A missing `)` is reported when the code is compiled.
5. **`xstrcat` does not work.** `= xstrcat(s, s)` fails with `wrong # args: should be "strcat imm8"`.
   The other three `x` functions work and are tested. (Mutants #266 and #267.)
6. **A debug value above 1 needs Beyond Compare.** `Calc::debug` writes two files under `$::env(TEMP)`
   and runs `C:/Program Files (x86)/Beyond Compare 2/BC2.exe`. On Linux it stops with
   `can't read "::env(TEMP)"`. The tests replace `Calc::debug` with a stub to check that it is
   called for `proc=`, `calc=`, `method=` and `transformProcs`, and not for debug values 0 and 1.
7. **Several `[: ...]` on one line.** If the line also contains a `[: {` form, the other brackets on
   that line stay interpreted (still correct, but not assembled). Lines with only unbraced forms
   are all assembled.
