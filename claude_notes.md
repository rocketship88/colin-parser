# claude_notes.md - orientation for a new Claude session

Read this first. It tells you what this project is, how it is put together, how to add a set of
operators for someone's own field, and which traps cost time before. Everything here was learned by
doing it; if something disagrees with the code or the tests, the code and tests win and this file
should be fixed.

## 1. What this is

**Calc** (the module `colon`, command `:` and alias `=`) compiles an expression language to Tcl
assembler (TAL) and runs it with `tcl::unsupported::assemble`. It is Colin Macleod's Pratt parser,
extended by Eric Taylor (rocketship88). Tcl 8.6 and 9.x, Linux and Windows.

```
: x = a * 2 + 1              ;# runtime: compiled on first use, cached by expression text
proc= f {a} { return [: a * 2 + 1] }       ;# compiled when the proc is defined
Calc::transformProcs ::ns::*               ;# compile the Calc code in existing procs
```

The point of the operator mechanism: Calc is a starting point for other people's small languages.
The hard parts (parsing, precedence, assembling, caching, namespaces, compiled procs) are done. A
new notation is a small module that calls `Calc::defineOperator`. Do not edit the core to add one.

## 2. Files

| File | What it is |
|---|---|
| `colon-1.0.tm` | The Tcl module: the whole compiler |
| `calcext.c` | The same thing as a C extension (`load ./calcext.so`). Provides package `colon` by itself |
| `calcvec-1.0.tm` | **The example operator set**: dot, cross, matrix and outer product. Copy this to make a new one |
| `calc.test` | Main test suite (tcltest). Operator tests are calc-57.* |
| `colon_test.tcl` | Second suite (a `verify` harness), plus timing |
| `calcvec.test` | Tests for calcvec. Use it as the model for the tests of a new set |
| `UserGuide.md`, `readme.md` | Documentation. "Defining Operators" in the UserGuide is the reference |
| `polyhedra.tcl`, `polyhedra-compare.html` | A larger converted program and its side by side page |
| `mutation/` | Mutation testing of `colon-1.0.tm` (see 7) |
| `calc.c` | Old extension that only provided `::`; history only |

The module and `calcext.c` must stay in step: same commands, same results, same error messages.
`Calc::defineOperator` exists twice (a Tcl proc in the module, a Tcl script string in `Calcext_Init`);
when one changes, change the other.

## 3. How the compiler works (enough to add operators)

`Calc::compile0` splits statements, `Calc::tokenise` cuts the text into tokens, `Calc::parse` is the
Pratt loop and returns TAL text, `parsePrefix` handles operands, prefix operators, calls and
parentheses. Operators live in tables in namespace `Calc`:

- `inprec(op)` precedence, `incode(op)` the TAL for built in infix operators, `precode(op)` prefix ones.
- Added by `defineOperator`: `opcmd(op)` the `tcl::mathop` command, `opinline(op)` the TAL template,
  `opspell(spelling)` the symbol it stands for, `opExtra` all added tokens longest first,
  `op_re` (module) the tokenizer regexp, `inlineOps` (1 = use templates, 0 = call the commands).
- Precedence scale: `,` `)` `=` 0, `?` 1, `||` 2, `&&` 3, `|` 4, `^` 5, `&` 6, `==` 7, `<` 8, `<<` 9,
  `+ -` 10, `* / %` 11, `**` 12, prefix 13. Binary operators are left associative (except `**`, `?`).
- `Calc::curns` is the namespace of the code being compiled. Function names and operator commands
  are looked up there first, then globally. Lookup order for `name(...)`: native table, `tcl::mathfunc::name`,
  a command, else an array.
- The compile cache `Calc::cache` is keyed by the expression text only.

## 4. How to add a set of operators

Make `NAME-1.0.tm` next to `colon-1.0.tm` (see `calcvec-1.0.tm`):

```tcl
package require colon
package provide NAME 1.0

proc ::tcl::mathop::foo {a b} { ... plain Tcl, the specification ... }
Calc::defineOperator \u2299 -name foo -prec 11 -spelled :foo:
```

Steps, in this order:

1. **Ask what the person needs**: the operations, whether they have standard symbols, the data layout
   (lists of numbers, lists of rows, ...), the sizes, the precedence they expect. Operators are
   infix, binary and left associative; for anything else use functions (`tcl::mathfunc::name`).
2. **Choose symbols** that exist in DejaVu Sans Mono (fixed width, free; many fonts lack the rarer
   ones) and that are not already built in. Give **every** operator a spelled form `:name:`, so a user
   without the font or the encoding can write plain ASCII.
3. **Write the proc first.** A plain `tcl::mathop` proc is correct, easy to read, and fast enough in
   almost every case. Use `foreach` over the lists, not index loops.
4. **Register it.** Write the symbol as `\uXXXX` in the `.tm` so the file is pure ASCII. Check the
   symbol with a `puts [format %x [scan $ch %c]]` style test, not by eye.
5. **Only if it is hot, add `-inline`** (section 5). A fixed size, loop free template is the case that
   pays (a 3 element dot product is 30% faster than the proc). A loop built by hand is not faster than
   a `foreach` proc on long lists.
6. **Write the tests** in the style of `calcvec.test`: known answers, the identities of the field
   (anti-commutativity, identity matrices, ...), error cases, compiled use inside a proc through
   `Calc::transformProcs`, ASCII `\uXXXX` in braces, the spelled forms, and for inline code a random
   comparison with the proc and a run with `set Calc::inlineOps 0`. Run them against the module and the
   C extension, on 8.6 and on 9.x.
7. **Document** the operators in a table: symbol, spelled form, meaning, how it is computed, and the
   precedence consequences (`2 * u ⊙ v` is `(2*u) ⊙ v`).

Typical requests and how they map: vector and matrix algebra is done in `calcvec`; set operations,
unit conversion, boolean algebra, finance (`a pct b`) and string operators are one proc each. A
transpose, a norm or any postfix notation becomes a function, not an operator.

## 5. Writing `-inline` templates

The contract: on entry the stack is `... left right` (right on top); on exit exactly one value, the
result, replaces both. Every `@` becomes this use's number (all `@` of one use are the same number, a
new number for the next use); `@@` is a literal `@`. A template that does not end in `;` gets one.

Rules that matter:
- Stay on the stack. No `load`/`store` of names: the template can not know the user's variables.
- `over N` copies the Nth value from the top (0 is the top). When a template builds several values the
  depth changes after each one; generate the template with a Tcl proc that tracks the depth (see
  `calcvec::el` and `calcvec::dot3code`; `-inline [calcvec::dot3code]` runs the proc once, at load).
- Branches need labels with `@` (`jumpFalse takeb@; ... label takeb@;`). Each name once per template.
- Instruction names: only those of `::tcl::unsupported::assemble`. 8.6 lacks some that 9.x has (no
  `swap`: use `reverse 2`). To list them, pass a bad name: the error message prints them all.
- The result type is a Tcl value; assemble does not check it. Test with odd input too (empty lists,
  strings, a missing element).
- Print the generated code with `Calc::compile0 {a :min: b}` and read it before you trust it.

## 6. Building and running the tests

All commands run in the folder that holds the files.

```
# module
tclsh calc.test ; tclsh colon_test.tcl ; tclsh calcvec.test
# Tcl 8.6 on Windows reads files in the system encoding; the tests with real symbols need:
tclsh86 -encoding utf-8 calcvec.test            (or source -encoding utf-8 calcvec.test)
# C extension (the tests look for a loaded calcext and use it): a one line driver
#   load ./calcext.so ; set f [lindex $argv 0] ; set argv {} ; source -encoding utf-8 $f
tclsh runc.tcl calc.test
# build (Linux), 8.6 from a source tree and 9.x with its stub library:
gcc -O2 -Wall -shared -fPIC -DUSE_TCL_STUBS -I$T/generic -I$T/unix -I$T/libtommath -o calcext.so calcext.c \
    $T/generic/tclStubLib.c $T/generic/tclTomMathStubLib.c
gcc -O2 -Wall -shared -fPIC -DUSE_TCL_STUBS -I<tcl9>/include -o calcext.so calcext.c -L<tcl9>/lib -ltclstub
```

Expected: `calc.test` total 380, 378 passed, 2 skipped (constraint `stackProblem`), 0 failed; `calcvec.test`
48 passed. `colon_test.tcl` reports one failure where the tcllib package `math` is missing. That is
the baseline, not a bug. Use `LC_ALL=C.UTF-8` on Linux so the test files with symbols are read as UTF-8.

## 7. Mutation testing

`mutation/` makes about 1165 small changes to `colon-1.0.tm` (`mutate.py`) and runs `calc.test`,
`colon_test.tcl` and `calcvec.test` against each (`run_mutants.py`); a mutant is killed when any output
changes. Run it after any change to the module:

```
cd mutation
python3 mutate.py ../colon-1.0.tm mutants.json
python3 run_mutants.py .. mutants.json results.csv --env LC_ALL=C.UTF-8 --jobs 2 --progress progress.json
```

It takes about 20-25 minutes per Tcl version on two cores (a background command stops after 30 minutes by default: give it a longer timeout and use `--resume`); a mutant that hangs a suite waits for the timeout (30 s).
If the run aborts, repeat the command with `--resume`: each result is appended to `results.csv.partial`
as it finishes. Run it in the background and watch the process, because it can die with the session.
Survivors are listed with reasons in `survivors.md`; a new survivor in new code usually means a missing test.
The C extension is not covered by mutation testing; it is held to the module by the identical tests.

## 8. Traps that cost time

- **Each assemble block must leave exactly one value** on the stack, and `:` statements chain.
- **`string match {*[ \t\n]*}` is a glob, not a regexp**: `\t` and `\n` there mean the letters `t` and `n`.
  Use `regexp {\s}`.
- **Unicode written by a tool**: after writing a file that must be ASCII (a `.tm`), run
  `grep -nP '[^\x00-\x7F]' file`. When a file must hold the real symbols (`calcvec.test`), check that
  the count of `\u` escapes is zero and the symbols are there. Print with `ascii()`, never by eye.
- **`puts` of a symbol** fails when stdout is not UTF-8 (Tcl 9 on a C locale). Use `LC_ALL=C.UTF-8`.
- **Math functions win over procs of the same name** in `name(...)` (`double`, `sqrt`...). Name test
  helpers carefully.
- **`method=` always sees the namespace `::oo::define`** (that is what `namespace current` is inside an
  `oo::define` body), so a bare name in a `method=` body finds global procs and `tcl::mathfunc`/`tcl::mathop`
  commands, not the procs of the class's namespace; qualify those. `proc=`, `calc=`, `:`, `=` and
  `Calc::transformProcs` do use the namespace of the code (calc-58 tests it).
- **A 1x1 matrix prints as one number**: `{{14}}` is the string `14`.
- **Literal lists are not Calc expressions**: write `(1, 2, 3)` or put the list in a variable.
- **Multiple statements need braces**: `: { x = 5; x + 1 }`. Unbraced, a `;` ends the command.
- **Operators first, then compile**: the cache is keyed by text; define operators before
  `Calc::transformProcs` and before the first `:` that uses them.
- **Two uses in one expression** must get different labels: that is why templates use `@`.
- `lsort -decreasing -command` of lengths puts the longest first; the tokenizer relies on it
  (`:ab:` and `:abc:`, `<+` and `<+>`).
- `Tcl_UniCharToUtf` and friends differ slightly between 8.6 and 9; build and test both.

## 9. Standing rules of this project

- Do not change the official Calc beyond what was asked; try ideas in a temporary copy.
- Deliver the project as `colin-parser-main.zip` (module, `calcext.c`, all test files, docs, `calcvec`,
  `polyhedra` files, `mutation/`, `.nojekyll`, `LICENSE`). GitHub is read only: never push.
- Never put the name of the author of the competing TIP series into the html comparison page; refer
  to the TIPs by number (768-775).
- Run the tests on 8.6 and 9.x, for the module and the C extension, before delivering. Check any claim
  about speed by timing it; the loop built by hand was slower than `foreach`, which was a surprise.
- Eric is 79, writes Tcl on Windows with Tcl 9.x wish and EditPad Pro 8. Explain things plainly,
  show a short example, and say what you did not test.

## 10. A request that a fresh session can pick up

> "Make me a `calcchem` module with operators for chemistry: `a -> b` ... "

1. Read sections 3-5 and `calcvec-1.0.tm`.
2. Ask only what blocks you: the operations, the symbols, the data layout.
3. Create `calcchem-1.0.tm` and `calcchem.test` from the `calcvec` files; run both suites on 8.6 and 9.x.
4. Add a table to the UserGuide (or a separate `calcchem.md`) and a line to the readme.
5. Deliver the new files, and the zip if the project changed. Do not touch `colon-1.0.tm`
   or `calcext.c` unless an operator needs something the mechanism lacks; if so, say so first.
