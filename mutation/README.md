# Mutation testing for colon-1.0.tm

Mutation testing checks the tests. A script makes one small change to the module (a mutant), runs
the test suites, and sees whether anything fails. A mutant that makes no test fail is a
**survivor**, and it points at something the tests do not check.

## Result

1002 mutants of `colon-1.0.tm` (the C extension `calc.c` is not covered).

| Tcl | killed | survived | share killed |
|---|---|---|---|
| 8.6.14 | 922 | 80 | 92.0% |
| 9.1.1 | 921 | 81 | 91.9% |

On 9.1.1 one more mutant survives (#374, which deletes the removal of `_` from `1_000`): Tcl 9 reads
`1_000` as a number itself, so the removal only matters on 8.6.

The numbers are for the module as it is now, after `&&` and `||` were changed to short circuit and
nested `?:` was made right associative. Before those two changes the module had 1003 mutants, and
the tests killed 923 of them (a first run with the older tests had killed 680). The changes removed
the old `&&`/`||` templates and added the jump code in `parse`; every mutant in the new code was
killed, and the two new survivors (#429 and #536, `parse 0` changed to `parse 1`) are equivalent now
that `?` keeps going at equal precedence. The 80 survivors are listed in `survivors.md`, each with
the reason I think it is equivalent or dead code.

By kind of change (8.6.14): `num` 237 of 269; `cmp` 103 of 113; `neg` 114 of 116; `del` 305 of 335; `swap` 108 of 109; `incr` 23 of 28; `opre` 32 of 32.

## Files

| File | What it is |
|---|---|
| `mutate.py` | Makes the mutants from `colon-1.0.tm` and writes `mutants.json` |
| `run_mutants.py` | Runs `calc.test` and `colon_test.tcl` against each mutant |
| `mutants.json` | The 1002 mutants (line number, kind, the edit) |
| `results.csv` | Status of every mutant on 8.6.14 and what killed it |
| `results-9.1.csv` | The same run on Tcl 9.1.1 |
| `survivors.md` | The 80 survivors, grouped by why they do not matter |
| `findings.md` | Behavior of the module found along the way |

## Running it

You need `python3` and `tclsh`. From this folder:

```
python3 mutate.py ../colon-1.0.tm mutants.json
python3 run_mutants.py .. mutants.json results.csv
```

`run_mutants.py` takes the folder that holds `colon-1.0.tm`, `calc.test` and `colon_test.tcl`. Options:
`--tclsh PATH` to use another interpreter, `--jobs N` (default 2), `--timeout SEC` (default 30),
`--only 1,2,3` to run only some mutants, `--env NAME=VALUE` (for example `TCL_LIBRARY`), and
`--progress FILE` to get a small JSON file with the count so far, and `--resume` to continue an
interrupted run (each result is also appended to `RESULTS.partial` as it finishes). About 1000 mutants take roughly
15 minutes on two cores, because a mutant that makes a suite loop runs until its timeout.

The line numbers in `mutants.json` belong to this version of `colon-1.0.tm`. If the module changes,
run `mutate.py` again.

## How a mutant is judged

- The unmodified module is run three times first, and its output must be the same each time.
- **KILLED**: the output of either suite differs from the unmodified output, or a suite hangs
  past the timeout.
- **SURVIVED**: both outputs are identical.
- **STILLBORN**: the mutated module does not load. None of the 1002 were.
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
