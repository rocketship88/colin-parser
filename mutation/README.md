# Mutation testing for colon-1.0.tm

Mutation testing checks the tests. A script makes one small change to the module (a mutant), runs
the test suites, and sees whether anything fails. A mutant that makes no test fail is a
**survivor**, and it points at something the tests do not check.

## Result

1168 mutants of `colon-1.0.tm` (the C extension `calcext.c` is not covered).

| Tcl | killed | survived | share killed |
|---|---|---|---|
| 8.6.14 | 1058 | 109 | 90.7% |
| 9.1.1 | 1057 | 110 | 90.6% |

One mutant (#249, which deletes the definition of the tokenizer's operator list) does not load and is
left out of the score. On 9.1.1 one more mutant survives (#487, which deletes the removal of `_` from
`1_000`): Tcl 9 reads `1_000` as a number itself, so the removal only matters on 8.6.

These numbers are for the module with `Calc::defineOperator` (the user defined operators), the
namespace lookup for function calls and the variable argument count for native functions. Before those
were added the module had 1002 mutants and 922 were killed on 8.6. Three suites are run against every
mutant: `calc.test`, `colon_test.tcl` and `calcvec.test` (the vector operators, which exercise the
inline code and the `@` labels through the module).

How the final numbers were made: all 1168 mutants were run on both versions. The survivors were read
(`survivors.md`); some pointed at real gaps, namely that nothing tested the places that record the
namespace of the compiled code (`:`, `=`, `proc=`, `calc=`, `method=`) with a fresh interpreter, that
the `\uXXXX` escape in expression text was never really exercised, and the redefinition and
longest-operator rules of `Calc::defineOperator`. Tests for those were added (`calc-57.37` to
`57.39`, `calc-57.40`, `57.41` and `calc-58`), and only the survivors were run again with the new
tests; 7 more mutants were killed on 8.6 and 7 on 9.1, and the table counts them as killed (the
`killed_by` column of the two csv files says "re-run with calc-58" for them). A mutant that was killed
stays killed when tests are added, so this gives the same result as running everything again. The
other 109 and 110 survivors are listed in `survivors.md`, each group with the reason I think it is
equivalent or dead code.

By kind of change (8.6.14): `num` 264 of 308; `cmp` 123 of 138; `neg` 137 of 140; `del` 363 of 404; `swap` 116 of 117; `incr` 23 of 28; `opre` 32 of 32.

## Files

| File | What it is |
|---|---|
| `mutate.py` | Makes the mutants from `colon-1.0.tm` and writes `mutants.json` |
| `run_mutants.py` | Runs `calc.test` and `colon_test.tcl` against each mutant |
| `mutants.json` | The 1168 mutants (line number, kind, the edit) |
| `results.csv` | Status of every mutant on 8.6.14 and what killed it |
| `results-9.1.csv` | The same run on Tcl 9.1.1 |
| `survivors.md` | The 110 survivors (on either version), grouped by why they do not matter |
| `findings.md` | Behavior of the module found along the way |

## Running it

You need `python3` and `tclsh`. From this folder:

```
python3 mutate.py ../colon-1.0.tm mutants.json
python3 run_mutants.py .. mutants.json results.csv
```

`run_mutants.py` takes the folder that holds `colon-1.0.tm`, `calc.test`, `colon_test.tcl`, `calcvec.test` and `calcvec-1.0.tm`. On Linux with Tcl 8.6 add `--env LC_ALL=C.UTF-8` so the test file with real symbols is read as UTF-8. Options:
`--tclsh PATH` to use another interpreter, `--jobs N` (default 2), `--timeout SEC` (default 30),
`--only 1,2,3` to run only some mutants, `--env NAME=VALUE` (for example `TCL_LIBRARY`), and
`--progress FILE` to get a small JSON file with the count so far, and `--resume` to continue an
interrupted run (each result is also appended to `RESULTS.partial` as it finishes). About 1170 mutants take roughly
25 minutes on two cores, because a mutant that makes a suite loop runs until its timeout.

The line numbers in `mutants.json` belong to this version of `colon-1.0.tm`. If the module changes,
run `mutate.py` again.

## How a mutant is judged

- The unmodified module is run three times first, and its output must be the same each time.
- **KILLED**: the output of either suite differs from the unmodified output, or a suite hangs
  past the timeout.
- **SURVIVED**: both outputs are identical.
- **STILLBORN**: the mutated module does not load. One of the 1168 was (#249, which deletes the operator list).
- The timing section at the end of `colon_test.tcl` is cut out of the temporary copy, and timing
  lines are ignored, so only the test results are compared.
- `colon_test.tcl` always reports one failure in an environment without the tcllib `math` package
  ("can't find package math"). That is part of the baseline, so it does not count as a kill.

## Kinds of change

- `swap`: an opcode or operator is replaced by its opposite (`add` and `sub`, `lt` and `gt`, `jumpTrue` and `jumpFalse`, ...)
- `num`: an integer literal goes up or down by 1
- `cmp`: a Tcl comparison or logic operator changes (`==` to `!=`, `&&` to `||`, `eq` to `ne`, ...)
- `del`: a whole statement is removed
- `neg`: an `if`, `elseif` or `while` condition is negated
- `incr`: `incr x` becomes `incr x -1`
- `opre`: one alternative is removed from the tokenizer regex

Comments, blank lines and the `Calc::debug` helper (which starts an external diff tool) are left alone.
