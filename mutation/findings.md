# Behavior found while writing the tests

Writing tests for the mutants that survived turned up the following. None of these was changed in
`colon-1.0.tm`. Two are in the test suites as skipped tests (constraint `knownBug`, set it to 1 near
the top of the section in `calc.test` to run them).

1. **Nested ternary without parentheses.** With a=1 b=2 c=3 d=4 e=5:
   - `= a ? b : c ? d : e` gives 4, `expr` gives 2. It is read as `(a ? b : c) ? d : e`.
   - `= a ? b ? c : d : e` fails with `Calc: expected ':' but found '?'`.
   - With parentheses both work: `a ? b : (c ? d : e)` and `a ? (b ? c : d) : e`.
   - Test: calc-45.7 (skipped).
2. **`&&` and `||` evaluate both sides.** `= 1 || 1/0` gives `divide by zero`, where `expr`
   returns 1. The operator tests avoid zero divisors for that reason. Test: calc-45.8 (skipped).
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
